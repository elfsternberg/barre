use arena::{Arena, Node, NodeId};
use consy::Cell;
use hashbrown::HashMap;
use parsesets::{ParseSet, ParseTreeExtractor, RedFn};
use render::render;
use std::rc::Rc;
use types::Parser;

#[derive(Copy, Clone)]
pub enum Nullable {
    Accept,
    Reject,
    InProgress,
    Unvisited,
}

pub struct NodePair<'a>(NodeId, &'a mut Node<Parser>);

pub struct Grammar {
    // The memory arena in which our graph is stored.  All "nodes" are
    // merely indexes into the arena to other nodes.
    pub arena: Arena<Parser>,

    // A parallel object storing whether or not the node is nullable or
    // has been proven nullable.  TODO: Roll this into the struct.
    pub nulls: Vec<Nullable>,

    // A collection of products of the parsing process.
    pub store: Vec<ParseSet>,

    // A memoized version of the (node, token) -> node tree, so that
    // in the event of recursion, we don't really recurse, we just
    // use this thing.
    pub memo: HashMap<(NodeId, char), NodeId>,

    // A map of nodes to the list of nodes which are interested in
    // their nullability.  When a node's nullability changes, all of
    // its children must be reconsidered, but this is much faster than
    // re-analyzing the entire nullability lattice with every
    // character.
    pub listeners: HashMap<NodeId, Vec<NodeId>>,

    // A convenience pointer to an empty parser.
    pub empty: NodeId,

    // The head node in the originating grammar
    pub start: NodeId,
}

// Deliberate mechanism to build things in the correct order, so that
// the compiler's habit of building temporaries is circumvented and
// the borrow rules work correctly.  Also: A lot easier.

macro_rules! make_node {
    ( $source:expr, $par:expr ) => {{
        $source.add($par)
    }};

    ( $source:expr, $par:expr, $lhs:expr ) => {{
        let newparser = make_node!($source, $par);
        $source.arena[newparser].left = $lhs;
        newparser
    }};

    ( $source:expr, $par:expr, $lhs:expr, $rhs:expr ) => {{
        let newparser = make_node!($source, $par, $lhs);
        $source.arena[newparser].right = $rhs;
        newparser
    }};
}

macro_rules! set_node {
    ( $target:expr, $par:expr ) => {{
        $target.data = $par;
    }};

    ( $target:expr, $par:expr, $lhs:expr ) => {{
        $target.left = $lhs;
        set_node!($target, $par);
    }};

    ( $target:expr, $par:expr, $lhs:expr, $rhs:expr ) => {{
        $target.right = $rhs;
        set_node!($target, $par, $lhs);
    }};
}

macro_rules! iff {
    ($condition: expr, $_true:expr, $_false:expr) => {
        if $condition {
            $_true
        } else {
            $_false
        }
    };
}

type ExtractionType = HashMap<(NodeId), ParseSet>;

// Default rules for various parsers.  Used to intialize the
// nullability tracking vector.  TODO: Roll this over to the parser
// types module.

pub fn parser_default_nullable(parser: &Parser) -> Nullable {
    match parser {
        Parser::Emp => Nullable::Reject,
        Parser::Eps(_) => Nullable::Accept,
        Parser::Tok(_) => Nullable::Reject,
        Parser::Alt => Nullable::Unvisited,
        Parser::Cat => Nullable::Unvisited,
        Parser::Del => Nullable::Unvisited,
        Parser::Red(_) => Nullable::Unvisited,
        Parser::Ukn => Nullable::Unvisited,
    }
}

pub fn init_nulls(arena: &Arena<Parser>) -> Vec<Nullable> {
    arena.iter().map(|t| parser_default_nullable(&t.data)).collect()
}

impl Grammar {
    pub fn new(arena: &Arena<Parser>, start: NodeId, empty: NodeId) -> Grammar {
        let nulls = init_nulls(&arena);
        let mut grammar = Grammar {
            arena: arena.clone(),
            nulls: nulls,
            store: vec![],
            memo: HashMap::new(),
            listeners: HashMap::new(),
            empty: empty,
            start: start
        };
        grammar.optimize(start);
        grammar
    }

    fn optimize_internal(&mut self, nodeid: NodeId, memo: &mut HashMap<NodeId, bool>) {
        if memo.get(&nodeid).unwrap_or(&false).clone() {
            return;
        }
        let node = self.arena[nodeid].clone();

        memo.insert(nodeid, true);
        let reoptimize = match node.data {
            Parser::Alt => {
                self.optimize_internal(node.left, memo);
                self.optimize_internal(node.right, memo);
                self.set_optimized_alt(nodeid, node.left, node.right)
            }
            
            Parser::Red(_) => {
                self.optimize_internal(node.left, memo);
                false
            }
            
            Parser::Cat => {
                self.optimize_internal(node.left, memo);
                self.optimize_internal(node.right, memo);
                let lopt = self.set_optimized_cat_left(nodeid, node.left, node.right);
                let ropt = {
                    let right = self.arena[node.right].clone();
                    match right.data {
                        Parser::Emp => {
                            set_node!(self.arena[nodeid], Parser::Emp);
                            true
                        }

                        Parser::Eps(ref n) => {
                            let closed_n = *n;
                            set_node!(
                                self.arena[nodeid],
                                Parser::Red(Rc::new(move |grammar, ts| {
                                    let ltree = grammar.fetch_cached_tree(closed_n);
                                    ltree.permute(&ts)
                                })),
                                node.right);
                            true
                        }

                        Parser::Red(ref g) => {
                            let gunc = g.clone();
                            set_node!(
                                self.arena[nodeid],
                                Parser::Red(Rc::new(
                                    move |grammar, ts| ts.run_after_floated_reduction(grammar, &gunc)
                                )),
                                self.make_optimized_cat(node.left, right.left)
                            );
                            true
                        }

                        _ => { false }
                    }
                };
                lopt || ropt
            }

            _ => { false }
        };

        if reoptimize {
            memo.insert(nodeid, false);
            self.optimize_internal(nodeid, memo);
        } else {
            self.nullable(nodeid);
        }
    }

    pub fn optimize(&mut self, nodeid: NodeId) {
        let mut memo: HashMap<NodeId, bool> = HashMap::new();
        self.optimize_internal(nodeid, &mut memo);
    }

    pub fn add(&mut self, parser: Parser) -> NodeId {
        self.nulls.push(parser_default_nullable(&parser));
        self.arena.add(parser)
    }

    //   ___             _               _
    //  / __|___ _ _  __| |_ _ _ _  _ __| |_ ___ _ _ ___
    // | (__/ _ \ ' \(_-<  _| '_| || / _|  _/ _ \ '_(_-<
    //  \___\___/_||_/__/\__|_|  \_,_\__|\__\___/_| /__/
    //

    // Given a token, builds and returns a new epsilon node that
    // contains a set with a parse tree of a single token.
    //
    pub fn make_eps(&mut self, token: &char) -> NodeId {
        self.store.push(ParseSet::with(Cell::Lit(token.clone())));
        let nodeid = self.store.len() - 1;
        self.add(Parser::Eps(nodeid))
    }

    pub fn make_kleene_star(&mut self, child_node: NodeId) -> NodeId {
        // L* = ε(s) ∪ (L ◦ L*)
        let ls = self.add(Parser::Ukn);
        let d = self.add(Parser::Cat);

        self.arena[d].left = child_node;
        self.arena[d].right = ls;

        self.arena[ls].left = self.make_eps(&char::default());
        self.arena[ls].right = d;
        self.arena[ls].data = Parser::Alt;
        ls
    }

    // Takes two child nodes and returns a new node built according to
    // the optimization function.
    //
    pub fn make_optimized_cat(&mut self, left_child_id: NodeId, right_child_id: NodeId) -> NodeId {
        let left = &self.arena[left_child_id].clone();

        match left.data {
            Parser::Emp => self.empty,
            Parser::Eps(ref n) => {
                let closed_n = *n;
                make_node!(
                    self,
                    Parser::Red(Rc::new(move |grammar, ts| {
                        let ltree = grammar.fetch_cached_tree(closed_n);
                        ltree.permute(&ts)
                    })),
                    right_child_id
                )
            }
            Parser::Cat => {
                let left_cat = make_node!(
                    self,
                    Parser::Cat,
                    left.left,
                    make_node!(self, Parser::Cat, left.right, right_child_id)
                );
                make_node!(
                    self,
                    Parser::Red(Rc::new(move |_grammar, ts| ts.rebalance_after_seq())),
                    left_cat
                )
            }
            _ => make_node!(self, Parser::Cat, left_child_id, right_child_id),
        }
    }

    // Takes a child node and a function, and returns a new node built
    // according to the optimization function.
    //
    fn make_optimized_red(&mut self, child_id: NodeId, func: Rc<RedFn>) -> NodeId {
        let child = self.arena[child_id].clone();
        match child.data {
            Parser::Emp => self.empty,
            Parser::Eps(ref n) => {
                let val = self.store[*n].clone();
                let res = func(self, val);
                self.store.push(res);
                let len = self.store.len() - 1;
                self.add(Parser::Eps(len))
            }
            Parser::Red(ref gunc) => {
                let f = func.clone();
                let g = gunc.clone();
                self.add(Parser::Red(Rc::new(move |grammar, ts| {
                    let t = g(grammar, ts);
                    f(grammar, t)
                })))
            }
            _ => make_node!(self, Parser::Red(func), child_id),
        }
    }

    // Takes a base node and two child nodes, and modifies the base
    // node, attaching children as needed.  Returns true if the
    // operation was an optimization.
    //
    fn set_optimized_alt(&mut self, target: NodeId, left_child_id: NodeId, right_child_id: NodeId) -> bool {
        let left = self.arena[left_child_id].clone();

        match left.data {
            // Optimization:     p   p
            Parser::Emp => {
                let right = &self.arena[right_child_id].clone();
                set_node!(self.arena[target], right.data.clone(), right.left, right.right);
                true
            }

            Parser::Eps(ref l) => {
                let right = self.arena[right_child_id].data.clone();
                match right {
                    // Optimization:  ε(s1) U ε(s2) -> (s1 U s2)
                    // Note that this optimization is the union of two parse forests.
                    Parser::Eps(ref r) => {
                        let pos = {
                            let ret = self.store[*l].union(&self.store[*r]);
                            self.store.push(ret);
                            self.store.len() - 1
                        };
                        set_node!(self.arena[target], Parser::Eps(pos));
                        true
                    }

                    // Default: Dc(L1   L2) = Dc(L1)   Dc(L2)
                    _ => {
                        set_node!(self.arena[target], Parser::Alt, left_child_id, right_child_id);
                        false
                    }
                }
            }

            _ => {
                let mut right = self.arena[right_child_id].clone();
                match right.data {
                    // Optimization: p       p
                    Parser::Emp => {
                        set_node!(self.arena[target], left.data.clone(), left.left, left.right);
                        true
                    }

                    // Default: Dc (L1   L2) = Dc (L1)   Dc (L2)
                    _ => {
                        set_node!(self.arena[target], Parser::Alt, left_child_id, right_child_id);
                        false
                    }
                }
            }
        }
    }

    // Takes a base node and two child nodes, and modifies the base
    // node, attaching children as needed.  Returns true if the
    // operation was an optimization.
    //
    // This is labeled "left" because optimizations on the right are
    // only done at build time.  Concatenation is the single hardest
    // aspect of this system to understand, so let me try to explain
    // how it works on the succesful match of mere tokens:
    // Cat(Lit('a'), Lit('b'))
    //
    // The only equations we'll be using are:
    // Dc(p   q) = (Dc(p)   q)   ( (p)   Dc(q)).
    //  (p) =  w. {(t, w) : t   [p])}
    // Dc(c') =  c if c == c'
    // Dc( c) =
    //
    // The second rule says that if the parser argument is a nullable
    // parser, return the parser, otherwise return null.  Null
    // concatenated with anything is null.
    //
    // So (p & q are parsers);
    // 1. A literal is not nullable.  Prese
    // D'a' = Dc('a')   q)
    // D'a' = ( 'a'   q)
    //
    // 2. Epsilon IS nullable:
    // D'a'D'b' = (    q)   ( ( 'a')   Dc("b"))
    // D'a'D'b' = ( 'a'    'b')
    //
    // See how the rules keep pushing the epsilons into the left hand
    // side of the union, where the   operator preserves them because
    // they're nullable?  It took me forever to understand that, and
    // now you understand it too.  But now we have to traverse those
    // nodes with every call to a cat parser, so one optimization is
    // to pack them all into a single epsilon.

    fn set_optimized_cat_left(&mut self, target: NodeId, left_child_id: NodeId, right_child_id: NodeId) -> bool {
        let left = self.arena[left_child_id].clone();
        match left.data {
            // Optimization:     p
            //
            Parser::Emp => {
                set_node!(self.arena[target], Parser::Emp);
                true
            }

            // Optimization:  s   p   p    u.(s, u)
            //
            // An epsilon on the left of a concatenation can
            // immediately be transformed into a reduction that
            // produces a list pair of the left epsilon with the
            // product of the right.  This eliminates the choresome
            // traversal of concatenative epsilons with each new
            // character.
            //
            Parser::Eps(ref n) => {
                let closed_n = *n;
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |grammar, ts| {
                        let ltree = grammar.fetch_cached_tree(closed_n);
                        ltree.permute(&ts)
                    })),
                    right_child_id
                );
                true
            }

            // Optimization: (p1   p2)   p3   (p1   (p2   p3))    u. {((t1, t2), t3) | (t1,(t2, t3))   u}
            //
            // This optimization is a bit tricky to understand, so here
            // goes my best explanation: Concatenation is associative,
            // that is, it does not matter in what order concatenative
            // pairs happen.  However, if the concatentation is heavy
            // on the left, the parser must descend the complete
            // left-hand tree with every iteration until the tree has
            // been consumed.
            //
            // This optimization says that it is better *mechanically*
            // to dangle the subsequent concatenations on the right,
            // so that we only process the tree *one level* per
            // character character.  After that, a reduction is
            // necessary to restore the concatenations to the order
            // specified in the grammar.  This would be irrelevant for
            // sequences of literals but for sequences of other
            // things, it's rather critical.
            //
            // This optimization only works in conjunction with the
            // epsilon collapse rule ( s   p   p    u.(s, u)), found
            // below, as otherwise we continue to accrue lists of
            // epsilons to traverse with every iteration.
            //
            Parser::Cat => {
                let deep_cat = self.make_optimized_cat(left.right, right_child_id);
                let left_optimized_cat = self.make_optimized_cat(left.left, deep_cat);
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |_grammar, ts| {
                        // println!("Rebalance from SOC: {:?}", ts);
                        ts.rebalance_after_seq()
                    })),
                    left_optimized_cat
                );
                true
            }
            // Optimization: (p1   f)   p2   (p1   p2)    u. {(f({t1}) , t2) | (t1, t2)   u}
            //
            // A reduction on the left can interfere with the
            // Parser::Cat reduction above.  This rule "floats" the
            // reduction up above the concatenation, leaving it to be
            // called during the extraction, and thus leaves the tree
            // in a cat-only condition suitable to optimization.
            //
            // TODO: Right now, this doesn't do anything at all,
            // because the infrastructure for user-defined assemblies
            // isn't done yet.
            Parser::Red(ref g) => {
                let gunc = g.clone();
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(
                        move |grammar, ts| ts.run_after_floated_reduction(grammar, &gunc)
                    )),
                    self.make_optimized_cat(left.left, right_child_id)
                );
                true
            }

            _ => {
                set_node!(self.arena[target], Parser::Cat, left_child_id, right_child_id);
                false
            }
        }
    }

    // Takes a base node, a child node, and a parnt node, and modifies
    // the base node, attaching children as needed.  Returns true if
    // the operation was an optimization.
    //
    fn set_optimized_red(&mut self, target: NodeId, child_id: NodeId, source_id: NodeId) -> bool {
        let child = self.arena[child_id].clone();
        match child.data {
            Parser::Emp => {
                self.arena[target].data = Parser::Emp;
                true
            }
            Parser::Eps(ref d) => {
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    let s = self.store[*d].clone();
                    let t = func(self, s);
                    self.store.push(t);
                    set_node!(self.arena[target], Parser::Eps(self.store.len() - 1));
                    true
                } else {
                    unreachable!()
                }
            }
            Parser::Red(ref gunc) => {
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    let f = func.clone();
                    let g = gunc.clone();
                    set_node!(
                        self.arena[target],
                        Parser::Red(Rc::new(move |grammar, ts| {
                            let t = g(grammar, ts);
                            f(grammar, t)
                        })),
                        child.left
                    );
                    true
                } else {
                    unreachable!()
                }
            }
            _ => {
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    set_node!(self.arena[target], Parser::Red(func.clone()), child_id);
                    false
                } else {
                    unreachable!()
                }
            }
        }
    }

    // Given a node and token, returns a node that represents the
    // derivative of the node passed in.
    //
    fn derive(&mut self, nodeid: NodeId, token: &char) -> NodeId {
        {
            if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
                return *cached_node;
            };
        };

        let node = &self.arena[nodeid].clone();
        let next_derivative = {
            match node.data {
                Parser::Emp => self.empty,
                Parser::Eps(_) => self.empty,
                Parser::Del => self.empty,
                Parser::Tok(ref t) => {
                    if *t == *token {
                        self.make_eps(token)
                    } else {
                        self.empty
                    }
                }
                Parser::Alt | Parser::Cat | Parser::Red(_) => self.add(Parser::Ukn),
                Parser::Ukn => unreachable!(),
            }
        };

        // Cache the result.  Since the cache value is just an index,
        // as long as we use the `set` functions that operate on what
        // that index points to, this is safe.
        //
        self.memo.insert((nodeid, token.clone()), next_derivative);

        let _ = match node.data {
            Parser::Alt => {
                let l = self.derive(node.left, token);
                let r = self.derive(node.right, token);
                self.set_optimized_alt(next_derivative, l, r)
            }

            Parser::Cat => {
                if self.nullable(node.left) {
                    let l = self.derive(node.left, token);
                    let r = self.derive(node.right, token);
                    let sac_l = node.left.clone();
                    let red = self.make_optimized_red(
                        r,
                        Rc::new(move |grammar, ts2| {
                            let ts1 = grammar.parse_tree(sac_l);
                            ts1.permute(&ts2)
                        }),
                    );
                    let cat = self.make_optimized_cat(l, node.right);
                    self.set_optimized_alt(next_derivative, red, cat)
                } else {
                    let l = self.derive(node.left, token);
                    self.set_optimized_cat_left(next_derivative, l, node.right)
                }
            }

            Parser::Red(_) => {
                let l = self.derive(node.left, token);
                self.set_optimized_red(next_derivative, l, nodeid)
            }

            _ => false,
        };

        next_derivative
    }

    //  _  _      _ _      _    _ _ _ _
    // | \| |_  _| | |__ _| |__(_) (_) |_ _  _
    // | .` | || | | / _` | '_ \ | | |  _| || |
    // |_|\_|\_,_|_|_\__,_|_.__/_|_|_|\__|\_, |
    //                                    |__/
    //
    fn nullable(&mut self, nodeid: NodeId) -> bool {
        // Strategy: Get a *copy* of the node, passing around a
        // reference kept alive during nullability testing. Always
        // associate a node with its ID.  When mutating the node,
        // mutate the local node then write a copy of it back to the
        // language.

        let mut node = self.arena[nodeid].clone();
        let mut node_pair = NodePair(nodeid, &mut node);
        self.cached_nullable(&mut node_pair, None, &Nullable::Unvisited)
    }

    fn cached_nullable(&mut self, nodepair: &mut NodePair, parent: Option<NodeId>, status: &Nullable) -> bool {
        let nullable = &self.nulls[nodepair.0].clone();
        match nullable {
            Nullable::Accept => true,
            Nullable::Reject => false,
            Nullable::InProgress => {
                if let Some(parent) = parent {
                    let mut listeners = self.listeners.entry(parent).or_insert(vec![]);
                    listeners.push(nodepair.0);
                }
                false
            }
            Nullable::Unvisited => {
                self.nulls[nodepair.0] = Nullable::InProgress;
                if self.compute_notify_nullable(nodepair, status) {
                    true
                } else {
                    // BUG? Are we manipulating a copy?
                    if let Some(parent) = parent {
                        let listeners = &mut self.listeners.entry(parent).or_insert(vec![]);
                        listeners.push(nodepair.0);
                    }
                    false
                }
            }
        }
    }

    fn compute_notify_nullable(&mut self, nodepair: &mut NodePair, status: &Nullable) -> bool {
        if self.base_nullable(nodepair, status) {
            {
                self.nulls[nodepair.0] = Nullable::Accept;
                if let Some((_, listeners)) = self.listeners.remove_entry(&nodepair.0) {
                    for childnode in listeners {
                        let mut node = self.arena[childnode].clone();
                        let mut newnp = NodePair(childnode.clone(), &mut node);
                        self.compute_notify_nullable(&mut newnp, &Nullable::Accept);
                    }
                }
                self.arena[nodepair.0] = nodepair.1.clone();
            }
            true
        } else {
            false
        }
    }

    fn base_nullable(&mut self, nodepair: &mut NodePair, status: &Nullable) -> bool {
        let nodeid = nodepair.0;
        match &nodepair.1.data {
            Parser::Emp => false,
            Parser::Eps(_) => true,
            Parser::Tok(_) => false,
            Parser::Alt => {
                let mut leftnode = self.arena[nodepair.1.left].clone();
                let mut rightnode = self.arena[nodepair.1.right].clone();
                self.cached_nullable(&mut NodePair(nodepair.1.left, &mut leftnode), Some(nodeid), status)
                    || self.cached_nullable(&mut NodePair(nodepair.1.right, &mut rightnode), Some(nodeid), status)
            }
            Parser::Cat => {
                let mut leftnode = self.arena[nodepair.1.left].clone();
                let mut rightnode = self.arena[nodepair.1.right].clone();
                self.cached_nullable(&mut NodePair(nodepair.1.left, &mut leftnode), Some(nodeid), status)
                    && self.cached_nullable(&mut NodePair(nodepair.1.right, &mut rightnode), Some(nodeid), status)
            }
            Parser::Del => true,
            Parser::Red(_) => {
                let mut leftnode = self.arena[nodepair.1.left].clone();
                self.cached_nullable(&mut NodePair(nodepair.1.left, &mut leftnode), Some(nodeid), status)
            }
            Parser::Ukn => unreachable!(),
        }
    }

    //  ___                    _____
    // | _ \__ _ _ _ ___ ___  |_   _| _ ___ ___
    // |  _/ _` | '_(_-</ -_)   | || '_/ -_) -_)
    // |_| \__,_|_| /__/\___|   |_||_| \___\___|
    //

    pub fn parse_tree_inner(&mut self, memo: &mut ExtractionType, nodeid: NodeId) -> ParseSet {
        if let Some(cached_result) = memo.get(&nodeid) {
            return cached_result.clone();
        };

        if !self.nullable(nodeid) {
            return ParseSet::new();
        };

        let node = &self.arena[nodeid].clone();

        match &node.data {
            Parser::Emp => ParseSet::new(),
            Parser::Tok(_) => ParseSet::new(),
            Parser::Eps(ref c) => self.store[*c].clone(),
            Parser::Del => self.parse_tree_inner(memo, node.left),

            Parser::Alt => {
                let p1 = self.parse_tree_inner(memo, node.left);
                p1.union(&self.parse_tree_inner(memo, node.right))
            }
            Parser::Cat => {
                let p1 = self.parse_tree_inner(memo, node.left);
                p1.permute(&self.parse_tree_inner(memo, node.right))
            }
            Parser::Red(ref f) => {
                let t_tree = self.parse_tree_inner(memo, node.left);
                (*f)(self, t_tree)
            }
            Parser::Ukn => {
                unreachable!("Uknnown node in parsetree operation. CANTHAPPEN");
            }
        }
    }

    //  ___                    _
    // | _ \__ _ _ _ ___ ___  | |
    // |  _/ _` | '_(_-</ -_) |_|
    // |_| \__,_|_| /__/\___| (_)
    //
    //
    pub fn parse<I>(&mut self, items: &mut I) -> Option<ParseSet>
    where
        I: Iterator<Item = char>,
    {
        let mut nodeid = self.start;
        let mut items = items.peekable();
        loop {
            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => {
                    let ret = self.nullable(nodeid);
                    break if ret { Some(self.parse_tree(nodeid)) } else { None };
                }

                Some(ref c) => {
                    let np = self.derive(nodeid, c);
                    let nl = &self.arena[np].data.clone();
                    match nl {
                        Parser::Emp => break None,
                        Parser::Eps(_) => match items.peek() {
                            Some(_) => break None,
                            None => break Some(self.parse_tree(np)),
                        },
                        // Essentially, for all other possibilities, we
                        // just need to recurse across our nodes until
                        // we hit Empty or Epsilon, and then we're
                        // done.
                        _ => {
                            nodeid = np;
                        }
                    }
                }
            }
        }
    }
}

impl ParseTreeExtractor for Grammar {
    fn parse_tree(&mut self, start: NodeId) -> ParseSet {
        let mut memo: ExtractionType = HashMap::new();
        self.parse_tree_inner(&mut memo, start)
    }

    fn fetch_cached_tree(&self, target: NodeId) -> ParseSet {
        self.store[target].clone()
    }
}
