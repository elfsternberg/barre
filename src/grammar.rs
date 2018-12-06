use arena::{Arena, NodeId};
use parsesets::{ParseSet, ParseTree, ParseTreeExtractor, RedFn};
use render::render;
use std::collections::HashMap;
use std::rc::Rc;
use types::Parser;

pub struct Grammar {
    pub arena: Arena<Parser>,
    pub store: Vec<ParseSet>,
    pub memo: HashMap<(NodeId, char), NodeId>,
    pub empty: NodeId,
}

// Deliberate mechanism to build things in the correct order, so that
// the compiler's habit of building temporaries is circumvented and
// the borrow rules work correctly.  Also: A lot easier.

macro_rules! add_node {
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

impl Grammar {
    fn add(&mut self, parser: Parser) -> NodeId {
        self.arena.add(parser)
    }

    fn add_eps(&mut self, token: &char) -> NodeId {
        self.store.push(ParseSet::with(ParseTree::Lit(token.clone())));
        self.arena.add(Parser::Eps(self.store.len() - 1))
    }

    fn add_optimized_cat(&mut self, left_child_id: NodeId, right_child_id: NodeId) -> NodeId {
        let left = &self.arena[left_child_id].clone();
        match left.data {
            Parser::Emp => self.empty,
            Parser::Cat => {
                let left_cat = add_node!(
                    self,
                    Parser::Cat,
                    left.left,
                    add_node!(self, Parser::Cat, left.right, right_child_id)
                );
                add_node!(
                    self,
                    Parser::Red(Rc::new(move |_grammar, ts| ts.rebalance_after_seq())),
                    left_cat
                )
            }
            Parser::Eps(ref n) => {
                let ltree = self.store[*n].clone();
                add_node!(self, Parser::Red(Rc::new(move |_grammar, ts| ltree.permute(&ts))))
            }
            _ => add_node!(self, Parser::Cat, left_child_id, right_child_id),
        }
    }

    fn add_optimized_red(&mut self, child_id: NodeId, func: Rc<RedFn>) -> NodeId {
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
            _ => add_node!(self, Parser::Red(func), child_id),
        }
    }

    fn set_optimized_alt(&mut self, target: NodeId, left_child_id: NodeId, right_child_id: NodeId) -> bool {
        let left = self.arena[left_child_id].clone();
        match left.data {
            Parser::Eps(ref l) => {
                let right = self.arena[right_child_id].data.clone();
                match right {
                    // Optimization:  ε(s1) U ε(s2)    (s1 U s2)
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

            // Optimization:     p   p
            Parser::Emp => {
                let right = &self.arena[right_child_id].clone();
                set_node!(self.arena[target], right.data.clone(), right.left, right.right);
                true
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
            Parser::Emp => {
                set_node!(self.arena[target], Parser::Emp);
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
                let deep_cat = self.add_optimized_cat(left.right, right_child_id);
                let left_optimized_cat = self.add_optimized_cat(left.left, deep_cat);
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |_grammar, ts| ts.rebalance_after_seq())),
                    left_optimized_cat
                );
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
                let ltree = self.store[*n].clone();
                set_node!(
                    self.arena[target],
                    Parser::Red(Rc::new(move |_grammar, ts| ltree.permute(&ts)))
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
                    Parser::Red(Rc::new(move |_grammar, ts| ts.run_after_floated_reduction(&gunc))),
                    self.add_optimized_cat(left.left, right_child_id)
                );
                true
            }

            _ => {
                set_node!(self.arena[target], Parser::Cat, left_child_id, right_child_id);
                false
            }
        }
    }

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
                    panic!("set_optimized_red called, but the caller was identified as not a reduction node.");
                }
            }
            Parser::Red(ref gunc) => {
                // [red-tag (red-node-set! node (node-child1 child) (compose1 func (node-child2 child))) #t]
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
                    panic!("set_optimized_red called, but the caller was identified as not a reduction node.");
                }
            }
            _ => {
                if let Parser::Red(ref func) = self.arena[source_id].data.clone() {
                    set_node!(self.arena[target], Parser::Red(func.clone()), child_id);
                    false
                } else {
                    panic!("set_optimized_red called, but the caller was identified as not a reduction node.");
                }
            }
        }
    }

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
                Parser::Tok(ref t) => iff!(*t == *token, self.add_eps(token), self.empty),
                Parser::Alt | Parser::Cat | Parser::Red(_) => self.add(Parser::Ukn),
                Parser::Ukn => {
                    panic!("Uknnown node in derive operation. CANTHAPPEN.");
                }
            }
        };

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
                    let red = self.add_optimized_red(
                        r,
                        Rc::new(move |grammar, ts2| {
                            let ts1 = grammar.parse_tree(sac_l);
                            ts1.permute(&ts2)
                        }),
                    );
                    let cat = self.add_optimized_cat(l, node.right);
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

    fn nullable(&mut self, nodeid: NodeId) -> bool {
        let node = &self.arena[nodeid].clone();

        match node.data {
            Parser::Emp => false,
            Parser::Eps(_) => true,
            Parser::Del => self.nullable(node.left),
            Parser::Tok(_) => false,
            Parser::Alt => self.nullable(node.left) || self.nullable(node.right),
            Parser::Cat => self.nullable(node.left) && self.nullable(node.right),
            //             Parser::Rep => true,
            Parser::Red(_) => self.nullable(node.left),
            Parser::Ukn => {
                panic!("Uknnown node in nullable operation. CANTHAPPEN.");
            }
        }
    }

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
                panic!("Uknnown node in parsetree operation. CANTHAPPEN");
            }
        }
    }

    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> Option<ParseSet>
    where
        I: Iterator<Item = char>,
    {
        let mut nodeid = start;
        let mut items = items.peekable();
        let mut count = 0;
        // println!("A1: {:?} {:?}", self.arena, nodeid);
        loop {
            render(&self.arena, nodeid, &format!("output-{:?}.dot", count));
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

impl ParseTreeExtractor for Grammar {
    fn parse_tree(&mut self, start: NodeId) -> ParseSet {
        let mut memo: ExtractionType = HashMap::new();
        self.parse_tree_inner(&mut memo, start)
    }
}
