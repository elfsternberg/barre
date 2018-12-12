use arena::{Arena, Node, NodeId};
use parsesets::{ParseSet, ParseTreeExtractor, RedFn};
use render::render;
use consy::Cell;
use hashbrown::{HashMap};
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
    pub arena: Arena<Parser>,
    pub nulls: Vec<Nullable>,
    pub store: Vec<ParseSet>,
    pub memo: HashMap<(NodeId, char), NodeId>,
    pub listeners: HashMap<NodeId, Vec<NodeId>>,
    pub empty: NodeId,
}

// Deliberate mechanism to build things in the correct order, so that
// the compiler's habit of building temporaries is circumvented and
// the borrow rules work correctly.  Also: A lot easier.

macro_rules! make_node {
    ( $source:expr, $par:expr ) => {{
        $source.add($par)
    }};

    ( $source:expr, $par:expr, $lhs:expr ) => {{
        let newparser = $source.add($par);
        $source.arena[newparser].left = $lhs;
        newparser
    }};

    ( $source:expr, $par:expr, $lhs:expr, $rhs:expr ) => {{
        let newparser = $source.add($par);
        let car = $lhs;
        let cdr = $rhs;
        $source.arena[newparser].left = car;
        $source.arena[newparser].right = cdr;
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


impl Grammar {
    pub fn add(&mut self, parser: Parser) -> NodeId {
        self.nulls.push(parser_default_nullable(&parser));
        self.arena.add(parser)
    }

    // Given a token, builds and returns a new epsilon node that
    // contains a set with a parse tree of a single token.
    //
    pub fn make_eps(&mut self, token: &char) -> NodeId {
        self.store.push(ParseSet::with(Cell::Lit(token.clone())));
        let nodeid = self.store.len() - 1;
        self.add(Parser::Eps(nodeid))
    }

    // Takes two child nodes and returns a new node built according to
    // the optimization function.
    //
    pub fn make_optimized_cat(&mut self, left_child_id: NodeId, right_child_id: NodeId) -> NodeId {
        let left = &self.arena[left_child_id].clone();

//        println!("MOC: Entered with {:?}: {:?}, {:?}: {:?}",
//                 left_child_id, self.arena[left_child_id], right_child_id, self.arena[right_child_id]);

        match left.data {
            Parser::Emp => {
                // println!("    MOC: Left Empty. Returning Empty.");
                self.empty
            },
            Parser::Eps(ref n) => {
                let closed_n = *n;
                // println!("    MOC: Left Epsilon. Returning Reduction pointing to {:?}", n);
                make_node!(
                    self,
                    Parser::Red(Rc::new(move |grammar, ts| {
                        let ltree = grammar.fetch_cached_tree(closed_n);
                        // println!("Running Red on: {:?}", ltree);
                        ltree.permute(&ts)
                    })),
                    right_child_id
                )
            }
            Parser::Cat => {
                // println!("    MOC: Left Cat. Returning rebalanced node with reduction.");
                let left_cat = make_node!(
                    self,
                    Parser::Cat,
                    left.left,
                    make_node!(self, Parser::Cat, left.right, right_child_id)
                );
                make_node!(
                    self,
                    Parser::Red(Rc::new(move |_grammar, ts| {
                        // println!("Rebalance from MOC: {:?}", ts);
                        ts.rebalance_after_seq()
                    })),
                    left_cat
                )
            }
            _ => {
                // println!("    MOC: Other: Returning ordinary cat node.");
                make_node!(self, Parser::Cat, left_child_id, right_child_id)
            }
        }
    }

    // Takes a child node and a function, and returns a new node built
    // according to the optimization function.
    //
    fn make_optimized_red(&mut self, child_id: NodeId, func: Rc<RedFn>) -> NodeId {
        let child = self.arena[child_id].clone();
        // println!("MOR: Entered with {:?}: {:?}",
        // child_id, self.arena[child_id]);
        match child.data {
            Parser::Emp => {
                // println!("    MOR: Child empty. Returning empty.");
                self.empty
            }
            Parser::Eps(ref n) => {
                // println!("    MOR: Child epsilon. Returning post-processed epsilon.");
                let val = self.store[*n].clone();
                let res = func(self, val);
                self.store.push(res);
                let len = self.store.len() - 1;
                self.add(Parser::Eps(len))
            }
            Parser::Red(ref gunc) => {
                // println!("    MOR: Child reduction. Returning composition");
                let f = func.clone();
                let g = gunc.clone();
                self.add(Parser::Red(Rc::new(move |grammar, ts| {
                    // println!("Compose from MOR: {:?}", ts);
                    let t = g(grammar, ts);
                    f(grammar, t)
                })))
            }
            _ => {
                // println!("    MOR: Other. Returning reduction node.");
                make_node!(self, Parser::Red(func), child_id)
            },
        }
    }

    // Takes a base node and two child nodes, and modifies the base
    // node, attaching children as needed.  Returns true if the
    // operation was an optimization.
    //
    fn set_optimized_alt(&mut self, target: NodeId, left_child_id: NodeId, right_child_id: NodeId) -> bool {
        let left = self.arena[left_child_id].clone();
        // println!("SOA: Entered with {:?}: {:?}, {:?}: {:?}",
        //         left_child_id, self.arena[left_child_id], right_child_id, self.arena[right_child_id]);

        match left.data {
            // Optimization:     p   p
            Parser::Emp => {
                // println!("  SOA: Left Empty. Returning right node.");
                let right = &self.arena[right_child_id].clone();
                set_node!(self.arena[target], right.data.clone(), right.left, right.right);
                true
            }

            Parser::Eps(ref l) => {
                // println!("  SOA: Left epsilon.");
                let right = self.arena[right_child_id].data.clone();
                match right {
                    // Optimization:  ε(s1) U ε(s2)    (s1 U s2)
                    // Note that this optimization is the union of two parse forests.
                    Parser::Eps(ref r) => {
                        // println!("      SOA: Right Eps. Building union.");
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
                        // println!("      SOA: Right other - making alt node.");
                        set_node!(self.arena[target], Parser::Alt, left_child_id, right_child_id);
                        false
                    }
                }
            }

            _ => {
                // println!("    SOA: Left other");
                let mut right = self.arena[right_child_id].clone();
                match right.data {
                    // Optimization: p       p
                    Parser::Emp => {
                        // println!("        SOA: Right Empty: Returning left node.");
                        set_node!(self.arena[target], left.data.clone(), left.left, left.right);
                        true
                    }

                    // Default: Dc (L1   L2) = Dc (L1)   Dc (L2)
                    _ => {
                        // println!("        SOA: Right Other - making alt node.");
                        // println!("Building ordinary alt with {:?}, {:?}", left_child_id, right_child_id);
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
        // println!("SOC: Entered with {:?}: {:?}, {:?}: {:?}",
        //         left_child_id, self.arena[left_child_id], right_child_id, self.arena[right_child_id]);
        let left = self.arena[left_child_id].clone();
        match left.data {
            // Optimization:     p
            //
            Parser::Emp => {
                // println!("    SOC: Left Empty. Returning Empty.");
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
                // println!("    SOC: Left Epsilon. Returning Reduction pointing to {:?}", n);
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |grammar, ts| {
                        let ltree = grammar.fetch_cached_tree(closed_n);
                        // println!("Running Red on: {:?}, {:?}", closed_n, ltree);
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
            // to dangle the susequent concatenations on the right, so
            // that we only process the tree *once* per character
            // character.  After that, a reduction is necessary to
            // restore the concatenations to the order specified in
            // the grammar.  This would be irrelevant for sequences of
            // literals but for sequences of other things, it's rather
            // critical.
            //
            // This optimization only works in conjunction with the
            // epsilon collapse rule ( s   p   p    u.(s, u)), found
            // below, as otherwise we continue to accrue lists of
            // epsilons to traverse with every iteration.
            //
            Parser::Cat => {
                // println!("    SOC: Left Cat. Returning rebalanced node with reduction.");
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
                // println!("    SOC: Left Red. Floating reduction above rebalancing.");
                let gunc = g.clone();
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |grammar, ts| {
                        // println!("Running Red on floated reduction after SOC: {:?}", ts);
                        ts.run_after_floated_reduction(grammar, &gunc)
                    })),
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
        // println!("SOR: Entered with {:?}: {:?}",
        //         child_id, self.arena[child_id]);
        match child.data {
            Parser::Emp => {
                // println!("    SOR: Child empty. Setting to empty.");
                self.arena[target].data = Parser::Emp;
                true
            }
            Parser::Eps(ref d) => {
                // println!("    SOR: Child epsilon. Returning post-processed epsilon.");
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
                // [red-tag (red-node-set! node (node-child1 child) (compose1 func (node-child2 child))) #t]
                // println!("    SOR: Child reduction. Returning composition");
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    // println!("Red Reduce: {:?} {:?}", source_id, child_id);
                    let f = func.clone();
                    let g = gunc.clone();
                    set_node!(
                        self.arena[target],
                        Parser::Red(Rc::new(move |grammar, ts| {
                            // println!("Compose! {:?}", ts);
                            let t = g(grammar, ts);
                            // println!("Result: {:?}", t);
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
                // println!("    SOR: Other. Returning reduction node.");
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    // println!("        SOR: Duplicating function for new node.");
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
        // println!("Processing node {:?}", nodeid);
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
                        let n = self.make_eps(token);
                        // println!("Making epsilon for {:?} at {:?}", token, n);
                        n
                    } else {
                        // println!("Token unrecognized, making empty");
                        self.empty
                    }
                },
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
                // println!("Building optimized alt! this: {:?} next: {:?}", nodeid, next_derivative);
                // println!("Before: {:?}, {:?}", self.arena[node.left], self.arena[node.right]);
                let l = self.derive(node.left, token);
                let r = self.derive(node.right, token);
                // println!("After: {:?}, {:?}", self.arena[l], self.arena[r]);
                self.set_optimized_alt(next_derivative, l, r)
            }

            Parser::Cat => {
                if self.nullable(node.left) {
                    // println!("{:03} Building nullable cat! next: {:?}, right: {:?}",
                    //          nodeid, self.arena[nodeid], next_derivative);

                    let l = self.derive(node.left, token);
                    let r = self.derive(node.right, token);
                    // println!("{:03} Continuing nullable cat! left:\n     {:?} {:?}\n    right: {:?} {:?}",
                    //          nodeid, l, self.arena[l], r, self.arena[r]);
                    
                    let sac_l = node.left.clone();
                    let red = self.make_optimized_red(
                        r,
                        Rc::new(move |grammar, ts2| {
                            let ts1 = grammar.parse_tree(sac_l);
                            // println!("Running Inner Red on: {:?} {:?}", sac_l, ts1);
                            ts1.permute(&ts2)
                        }),
                    );
                    let cat = self.make_optimized_cat(l, node.right);
                    self.set_optimized_alt(next_derivative, red, cat)
                } else {
                    // println!("{:03} Building optimized cat! next: {:?}, right: {:?}", nodeid, next_derivative, self.arena[node.right]);
                    let l = self.derive(node.left, token);
                    // println!("{:03} Building optimized cat! left: {:?}", nodeid, self.arena[l]);
                    self.set_optimized_cat_left(next_derivative, l, node.right)
                }
            }

            Parser::Red(_) => {
                // println!("Deriving reduction {:?}", nodeid);
                let l = self.derive(node.left, token);
                self.set_optimized_red(next_derivative, l, nodeid)
            }

            _ => false,
        };

        // println!("Intermediate made for {:?}", next_derivative);
        // self.dump(next_derivative);
        // println!("");
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
                } else {
                    unreachable!();
                }
                false
            }
            Nullable::Unvisited => {
                self.nulls[nodepair.0] = Nullable::InProgress;
                if self.compute_notify_nullable(nodepair, status) {
                    true
                } else {
                    if let Some(parent) = parent {
                        let mut listeners = self.listeners.entry(parent).or_insert(vec![]);
                        listeners.push(nodepair.0);
                    }
                    false
                }
            }
        }
    }

    fn set_nullable_and_mark(&mut self, nodepair: &mut NodePair, status: &Nullable) -> bool {
        self.nulls[nodepair.0] = status.clone();
        if let Some((_, listeners)) = self.listeners.remove_entry(&nodepair.0) {
            for childnode in listeners {
                let mut node = self.arena[childnode].clone();
                let mut newnp = NodePair(childnode.clone(), &mut node);
                self.compute_notify_nullable(&mut newnp, status);
            }
        }

        // Big dangerous mutation here.
        self.arena[nodepair.0] = nodepair.1.clone();
        true
    }

    fn compute_notify_nullable(&mut self, nodepair: &mut NodePair, status: &Nullable) -> bool {
        if self.might_nullable(nodepair, status) {
            self.set_nullable_and_mark(nodepair, &Nullable::Accept)
        } else {
            false
        }
    }

    fn might_nullable(&mut self, nodepair: &mut NodePair, status: &Nullable) -> bool {
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
            Parser::Ukn => {
                unreachable!()
            }
        }
    }

    //  ___                    _____            
    // | _ \__ _ _ _ ___ ___  |_   _| _ ___ ___ 
    // |  _/ _` | '_(_-</ -_)   | || '_/ -_) -_)
    // |_| \__,_|_| /__/\___|   |_||_| \___\___|
    //                                          
    
    pub fn parse_tree_inner(&mut self, memo: &mut ExtractionType, nodeid: NodeId) -> ParseSet {
        // println!("{:?}", self.store);
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
    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> Option<ParseSet>
    where
        I: Iterator<Item = char>,
    {
        let mut nodeid = start;
        let mut items = items.peekable();
        let mut count = 0;
        loop {
            // render(&self.arena, nodeid, &format!("output-{:?}.dot", count));
            // println!("CHAR: {:?}", items.peek());
            // self.dump(nodeid);
            count += 1;
            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => {
                    render(&self.arena, nodeid, &format!("output-{:?}.dot", count));
                    // println!("Sending to breaker: {:?}", nodeid);
                    let ret = self.nullable(nodeid);
                    render(&self.arena, nodeid, &format!("output-{:?}.dot", count + 1));
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

impl Grammar {
    pub fn inner_dump(&self, nodeid: NodeId, depth: usize) {
        let node = self.arena[nodeid].clone();
        let lead = format!("{:03} {lead:>width$}", nodeid, lead="", width=depth * 4);
        match node.data {
            Parser::Emp => println!("{}{}", lead, "Emp"),
            Parser::Eps(ref n) => println!("{}{}: {:?}", lead, "Eps", &self.store[*n]),
            Parser::Tok(c) => println!("{}{}{}", lead, "Tok: ", c),
            Parser::Alt => {
                println!("{}{}", lead, "Alt");
                self.inner_dump(node.left, depth + 1);
                self.inner_dump(node.right, depth + 1);
            },
                
            Parser::Cat => {
                println!("{}{}", lead, "Cat");
                self.inner_dump(node.left, depth + 1);
                self.inner_dump(node.right, depth + 1);
            },
                
            Parser::Del => println!("{}{}", lead, "Del"),
            Parser::Red(_) => {
                println!("{}{}", lead, "Red");
                self.inner_dump(node.left, depth + 1);
            },

            Parser::Ukn => println!("{}{}", lead, "Ukn"),
        };
    }

    pub fn dump(&self, nodeid: NodeId) {
        self.inner_dump(nodeid, 0);
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
