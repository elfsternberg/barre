use arena::{Arena, Node, NodeId};

use consy::{Cell, cons};
use hashbrown::HashMap;

use indexmap::indexset;
use language::Language;
use siaa::{Riaa, Siaa};
use std::iter::FromIterator;
use std::ops::{Index, IndexMut};
use std::rc::Rc;
use types::Parser;

use parsesets::{Forest, ParseTreeExtractor, RedFn};

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

/// A grammar represent the list of rules that describe
/// a regular expression: they're the individual atoms
/// of concatenate, alternate, repeat, token, empty and
/// null.

#[derive(Clone)]
pub struct Grammar<T: Siaa + 'static, U: Riaa<T> + 'static>
where
    U: std::convert::From<T>,
{
    // The memory arena in which our graph is stored.  All "nodes" are
    // merely indexes into the arena to other nodes.
    pub arena: Arena<Parser<T, U>>,

    // While it's possible to just have a lot of empties, they're
    // utterly indistinguishable from one another, so having just one
    // every empty result can point to is a small bit of savings.
    pub empty: NodeId,

    // Every language has a starting point, the head of the tree
    // of processing instructions.
    pub start: NodeId,

    // Pre-reduction cast operator
    pub cast: Rc<Fn(T) -> U>,
}

impl<T: Siaa, U: Riaa<T>> Grammar<T, U>
where
    U: std::convert::From<T>,
{
    pub fn raw() -> Grammar<T, U> {
        Grammar {
            arena: Arena::new(),
            empty: 0,
            start: 0,
            cast: Rc::new(|x| T::into(x)),
        }
    }

    pub fn new() -> Grammar<T, U> {
        let mut grammar = Grammar::raw();
        let _ = grammar.make_emp();
        grammar.empty = grammar.make_emp();
        grammar.start = grammar.empty;
        grammar
    }

    pub fn from_language(lang: &Language<T>) -> Grammar<T, U> {
        let mut grammar = Grammar::new();

        fn language_handler<T: Siaa, U: Riaa<T>>(lang: &Language<T>, g: &mut Grammar<T, U>) -> NodeId
        where
            U: std::convert::From<T>,
        {
            match lang {
                Language::Epsilon => g.make_eps_from_token(&T::default()),

                Language::Token(ref t) => g.make_tok(&t.0.clone()),

                Language::Alt(ref node) => {
                    let l = language_handler(&node.0, g);
                    let r = language_handler(&node.1, g);
                    g.make_alt(l, r)
                }

                Language::Cat(ref node) => {
                    let l = language_handler(&node.0, g);
                    let r = language_handler(&node.1, g);
                    g.make_cat(l, r)
                }
            }
        }

        let start = language_handler(&lang, &mut grammar);
        grammar.start = start;
        grammar
    }

    //  _  _         _        ___             _               _
    // | \| |___  __| |___   / __|___ _ _  __| |_ _ _ _  _ __| |_ ___ _ _ ___
    // | .` / _ \/ _` / -_) | (__/ _ \ ' \(_-<  _| '_| || / _|  _/ _ \ '_(_-<
    // |_|\_\___/\__,_\___|  \___\___/_||_/__/\__|_|  \_,_\__|\__\___/_| /__/
    //
    //
    pub fn make_emp(&mut self) -> NodeId {
        make_node!(self.arena, Parser::Emp)
    }

    pub fn make_eps(&mut self, token: &Rc<Forest<U>>) -> NodeId {
        make_node!(self.arena, Parser::Eps(token.clone()))
    }

    pub fn make_eps_from_token(&mut self, token: &T) -> NodeId {
        let val = (*self.cast)(token.clone());
        self.make_eps(&Rc::new(Forest(indexset!(Cell::Lit(val)))))
    }

    pub fn make_tok(&mut self, token: &T) -> NodeId {
        make_node!(self.arena, Parser::Tok(token.clone()))
    }

    pub fn make_alt(&mut self, left: NodeId, right: NodeId) -> NodeId {
        make_node!(self.arena, Parser::Alt, left, right)
    }

    pub fn make_cat(&mut self, left: NodeId, right: NodeId) -> NodeId {
        make_node!(self.arena, Parser::Cat, left, right)
    }

    pub fn make_red(&mut self, left: NodeId, func: Rc<RedFn<T, U>>) -> NodeId {
        make_node!(self.arena, Parser::Red(func), left)
    }

    pub fn make_ukn(&mut self) -> NodeId {
        make_node!(self.arena, Parser::Ukn)
    }

    // Kleene star, only using recursion instead of iteration.
    pub fn make_rep(&mut self, child_node: NodeId) -> NodeId {
        // L* = ε() ∪ (L ◦ L*)
        let lstar = self.make_ukn();
        let right = self.make_cat(child_node, lstar);
        let epsto = self.make_eps_from_token(&T::default());
        self.set_alt(lstar, epsto, right);
        lstar
    }

    //  _  _         _       __  __      _        _
    // | \| |___  __| |___  |  \/  |_  _| |_ __ _| |_ ___ _ _ ___
    // | .` / _ \/ _` / -_) | |\/| | || |  _/ _` |  _/ _ \ '_(_-<
    // |_|\_\___/\__,_\___| |_|  |_|\_,_|\__\__,_|\__\___/_| /__/
    //
    pub fn set_emp(&mut self, target: NodeId) {
        set_node!(self.arena[target], Parser::Emp);
    }

    pub fn set_eps(&mut self, target: NodeId, token: &Rc<Forest<U>>) {
        set_node!(self.arena[target], Parser::Eps(token.clone()));
    }

    pub fn set_eps_from_token(&mut self, target: NodeId, token: T) {
        let val = (*self.cast)(token.clone());
        self.set_eps(target, &Rc::new(Forest(indexset!(Cell::Lit(val)))));
    }

    pub fn set_tok(&mut self, target: NodeId, token: &T) {
        set_node!(self.arena[target], Parser::Tok(token.clone()));
    }

    pub fn set_alt(&mut self, target: NodeId, left: NodeId, right: NodeId) {
        set_node!(self.arena[target], Parser::Alt, left, right);
    }

    pub fn set_cat(&mut self, target: NodeId, left: NodeId, right: NodeId) {
        set_node!(self.arena[target], Parser::Cat, left, right);
    }

    pub fn set_red(&mut self, target: NodeId, left: NodeId, func: Rc<RedFn<T, U>>) {
        set_node!(self.arena[target], Parser::Red(func), left);
    }

    pub fn set_ukn(&mut self, target: NodeId) {
        set_node!(self.arena[target], Parser::Ukn);
    }
}

impl<T: Siaa, U: Riaa<T>> Index<NodeId> for Grammar<T, U>
where
    U: std::convert::From<T>,
{
    type Output = Node<Parser<T, U>>;
    fn index<'a>(&'a self, index: NodeId) -> &'a Node<Parser<T, U>> {
        debug_assert!(index < self.arena.len());
        &self.arena[index]
    }
}

impl<T: Siaa, U: Riaa<T>> IndexMut<NodeId> for Grammar<T, U>
where
    U: std::convert::From<T>,
{
    fn index_mut<'a>(&'a mut self, index: NodeId) -> &'a mut Node<Parser<T, U>> {
        debug_assert!(index < self.arena.len());
        &mut self.arena[index]
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Nullable {
    Accept,
    Reject,
    InProgress,
    Unvisited,
}

pub struct NodePair<'a, T: Siaa, U: Riaa<T>>(NodeId, &'a mut Node<Parser<T, U>>);

pub struct Deriver<T: Siaa + 'static, U: Riaa<T> + 'static>
where
    U: std::convert::From<T>,
{
    // The memory arena in which our graph is stored.  All "nodes" are
    // merely indexes into the arena to other nodes.
    pub grammar: Grammar<T, U>,

    // A memoized version of the (node, token) -> node tree, so that
    // in the event of recursion, we don't really recurse, we just
    // use this thing.
    pub memo: HashMap<(NodeId, T), NodeId>,

    // A parallel object storing whether or not the node is nullable or
    // has been proven nullable.
    pub nulls: HashMap<NodeId, Nullable>,

    // A map of nodes to the list of nodes which are interested in
    // their nullability.  When a node's nullability changes, all of
    // its children must be reconsidered, but this is much faster than
    // re-analyzing the entire nullability lattice with every
    // character.
    pub listeners: HashMap<NodeId, Vec<NodeId>>,
}

type ExtractionType<U> = HashMap<NodeId, Forest<U>>;

// Default rules for various parsers.  Used to intialize the
// nullability tracking vector.  TODO: Roll this over to the parser
// types module.

pub fn parser_default_nullable<T: Siaa, U: Riaa<T>>(parser: &Parser<T, U>) -> Nullable {
    match parser {
        Parser::Emp => Nullable::Reject,
        Parser::Eps(_) => Nullable::Accept,
        Parser::Tok(_) => Nullable::Reject,
        Parser::Alt => Nullable::Unvisited,
        Parser::Cat => Nullable::Unvisited,
        Parser::Red(_) => Nullable::Unvisited,
        Parser::Ukn => Nullable::Unvisited,
    }
}

pub fn init_nulls<T: Siaa, U: Riaa<T>>(arena: &Arena<Parser<T, U>>) -> HashMap<NodeId, Nullable>
where
    U: std::convert::From<T>,
{
    HashMap::from_iter(
        arena
            .iter()
            .enumerate()
            .map(|(i, t)| (i, parser_default_nullable(&t.data))),
    )
}

macro_rules! make_with_null {
    ($s:expr, $e:expr) => {{
        let n = $e;
        $s.nulls.insert(n, parser_default_nullable(&$s.grammar[n].data));
        n
    }};
}

// Functions used by the optimizer to, um, optimize.
//
pub fn union<T, U: Riaa<T>>(this: &Forest<U>, other: &Forest<U>) -> Forest<U>
where
    U: std::convert::From<T>,
{
    Forest(other.0.union(&this.0).cloned().collect())
}

pub fn permute<T, U: Riaa<T>>(this: &Forest<U>, other: &Forest<U>) -> Forest<U>
where
    U: std::convert::From<T>,
{
    let mut ret = Forest::new();
    for t1 in &this.0 {
        for t2 in &other.0 {
            ret.0.insert(cons!(t1.clone(), t2.clone()));
        }
    }
    ret
}

// Optimization: (p1 ◦ p2) ◦ p3 ⇒ (p1 ◦ (p2 ◦ p3)) → λu. {((t1, t2), t3) | (t1,(t2, t3)) ∈ u}
// See grammar::Grammer::set_optimized_cat_left for details.
//
pub fn rebalance_after_seq<T, U: Riaa<T>>(this: &Forest<U>) -> Forest<U>
where
    U: std::convert::From<T>,
{
    let mut ret = Forest::new();
    for t1 in &this.0 {
        ret.insert(if t1.pairp() && t1.cdr().unwrap_or(&Cell::Nil).pairp() {
            cons!(
                cons!(t1.car().unwrap().clone(), t1.cadr().unwrap().clone()),
                t1.cddr().unwrap().clone()
            )
        } else {
            // Because this is a reconstruction after an optimization,
            // the above structure must be correct.  If it's not,
            // we've got bigger problems.
            unreachable!()
        })
    }
    ret
}

pub fn run_after_floated_reduction<T: Siaa, U: Riaa<T>>(
    grammar: &mut ParseTreeExtractor<T, U>,
    this: &Forest<U>,
    func: &Rc<RedFn<T, U>>,
) -> Forest<U>
where
    U: std::convert::From<T>,
{
    let mut ret = Forest::new();
    for t1 in &this.0 {
        for t2 in func(grammar, &Forest(indexset!(t1.car().unwrap().clone()))).0 {
            ret.insert(cons!(t2, t1.cdr().unwrap().clone()))
        }
    }
    ret
}

impl<T: Siaa, U: Riaa<T>> Deriver<T, U>
where
    U: std::convert::From<T>,
{
    pub fn new(grammar: &Grammar<T, U>) -> Deriver<T, U> {
        let nulls = init_nulls(&grammar.arena);
        let mut deriver = Deriver {
            grammar: grammar.clone(),
            nulls: nulls,
            memo: HashMap::new(),
            listeners: HashMap::new(),
        };
        deriver.optimize(grammar.start);
        deriver
    }

    pub fn make_emp(&mut self) -> NodeId {
        make_with_null!(self, self.grammar.make_emp())
    }

    pub fn make_eps(&mut self, token: &Rc<Forest<U>>) -> NodeId {
        make_with_null!(self, self.grammar.make_eps(token))
    }

    pub fn make_eps_from_token(&mut self, token: &T) -> NodeId {
        let val = (*self.grammar.cast)(token.clone());
        self.make_eps(&Rc::new(Forest(indexset!(Cell::Lit(val)))))
    }

    pub fn make_tok(&mut self, token: &T) -> NodeId {
        make_with_null!(self, self.grammar.make_tok(token))
    }

    pub fn make_alt(&mut self, left: NodeId, right: NodeId) -> NodeId {
        make_with_null!(self, self.grammar.make_alt(left, right))
    }

    pub fn make_cat(&mut self, left: NodeId, right: NodeId) -> NodeId {
        make_with_null!(self, self.grammar.make_cat(left, right))
    }

    pub fn make_red(&mut self, left: NodeId, func: Rc<RedFn<T, U>>) -> NodeId {
        make_with_null!(self, self.grammar.make_red(left, func))
    }

    pub fn make_ukn(&mut self) -> NodeId {
        make_with_null!(self, self.grammar.make_ukn())
    }

    fn optimize_internal(&mut self, nodeid: NodeId, memo: &mut HashMap<NodeId, bool>) {
        if memo.get(&nodeid).unwrap_or(&false).clone() {
            return;
        }
        let node = self.grammar[nodeid].clone();

        memo.insert(nodeid, true);
        let reoptimize = match node.data {
            Parser::Alt => {
                self.optimize_internal(node.left, memo);
                self.optimize_internal(node.right, memo);
                self.set_optimized_alt(nodeid, node.left, node.right)
            }

            Parser::Cat => {
                self.optimize_internal(node.left, memo);
                self.optimize_internal(node.right, memo);
                let lopt = self.set_optimized_cat_left(nodeid, node.left, node.right);
                let ropt = {
                    let right = self.grammar[node.right].clone();
                    match right.data {
                        Parser::Emp => {
                            self.grammar.set_emp(nodeid);
                            true
                        }

                        Parser::Eps(ref eps) => {
                            let eps_rc = eps.clone();
                            self.grammar.set_red(
                                nodeid,
                                node.right,
                                Rc::new(move |_grammar, ts| permute(&*eps_rc, &ts)),
                            );
                            true
                        }

                        Parser::Red(ref g) => {
                            let gunc = g.clone();
                            let child_node = self.make_optimized_cat(node.left, right.left);
                            self.grammar.set_red(
                                nodeid,
                                child_node,
                                Rc::new(move |grammar, ts| run_after_floated_reduction(grammar, ts, &gunc)),
                            );
                            true
                        }

                        _ => false,
                    }
                };
                lopt || ropt
            }

            // TODO: This is incomplete. Look into why.
            Parser::Red(_) => {
                self.optimize_internal(node.left, memo);
                false
            }

            _ => false,
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

    // Takes two child nodes and returns a new node built according to
    // the optimization function.
    //
    pub fn make_optimized_cat(&mut self, left_child_id: NodeId, right_child_id: NodeId) -> NodeId {
        let left = &self.grammar[left_child_id].clone();

        match left.data {
            Parser::Emp => self.grammar.empty,

            Parser::Eps(ref eps) => {
                let closed_pt = eps.clone();
                self.make_red(right_child_id, Rc::new(move |_grammar, ts| permute(&*closed_pt, &ts)))
            }

            Parser::Cat => {
                let left_right_cat = self.make_cat(left.right, right_child_id);
                let left_cat = self.make_cat(left.left, left_right_cat);
                self.make_red(left_cat, Rc::new(move |_grammar, ts| rebalance_after_seq(&ts)))
            }

            _ => self.make_cat(left_child_id, right_child_id),
        }
    }

    // Takes a child node and a function, and returns a new node built
    // according to the optimization function.
    //
    fn make_optimized_red(&mut self, child_id: NodeId, func: Rc<RedFn<T, U>>) -> NodeId {
        let child = self.grammar[child_id].clone();

        match child.data {
            Parser::Emp => self.grammar.empty,

            Parser::Eps(ref eps) => {
                let res = func(self, &*eps);
                self.make_eps(&Rc::new(res))
            }

            Parser::Red(ref gunc) => {
                let f = func.clone();
                let g = gunc.clone();
                self.make_red(
                    child.left,
                    Rc::new(move |grammar, ts| {
                        let t = g(grammar, ts);
                        f(grammar, &t)
                    }),
                )
            }

            _ => self.make_red(child_id, func),
        }
    }

    // Takes a base node and two child nodes, and modifies the base
    // node, attaching children as needed.  Returns true if the
    // operation was an optimization.
    //
    fn set_optimized_alt(&mut self, target: NodeId, left_child_id: NodeId, right_child_id: NodeId) -> bool {
        let left = self.grammar[left_child_id].clone();

        match left.data {
            // Optimization:     p   p
            Parser::Emp => {
                let right = &self.grammar[right_child_id].clone();
                set_node!(self.grammar[target], right.data.clone(), right.left, right.right);
                true
            }

            Parser::Eps(ref leps) => {
                let right = self.grammar[right_child_id].data.clone();
                match right {
                    // Optimization:  ε(s1) U ε(s2) -> (s1 U s2)
                    // Note that this optimization is the union of two parse forests.
                    Parser::Eps(ref reps) => {
                        let new_eps = union(&leps, &reps);
                        self.grammar.set_eps(target, &Rc::new(new_eps));
                        true
                    }

                    // Default: Dc(L1   L2) = Dc(L1)   Dc(L2)
                    _ => {
                        self.grammar.set_alt(target, left_child_id, right_child_id);
                        false
                    }
                }
            }

            _ => {
                let mut right = self.grammar[right_child_id].clone();
                match right.data {
                    // Optimization: p       p
                    Parser::Emp => {
                        set_node!(self.grammar[target], left.data.clone(), left.left, left.right);
                        true
                    }

                    // Default: Dc (L1   L2) = Dc (L1)   Dc (L2)
                    _ => {
                        self.grammar.set_alt(target, left_child_id, right_child_id);
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
        let left = self.grammar[left_child_id].clone();
        match left.data {
            // Optimization:     p
            //
            Parser::Emp => {
                self.grammar.set_emp(target);
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
            Parser::Eps(ref eps) => {
                let closed_eps = eps.clone();
                self.grammar.set_red(
                    target,
                    right_child_id,
                    Rc::new(move |_grammar, ts| permute(&closed_eps, &ts)),
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
                self.grammar.set_red(
                    target,
                    left_optimized_cat,
                    Rc::new(move |_grammar, ts| rebalance_after_seq(&ts)),
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
                let child = self.make_optimized_cat(left.left, right_child_id);
                self.grammar.set_red(
                    target,
                    child,
                    Rc::new(move |grammar, ts| run_after_floated_reduction(grammar, &ts, &gunc)),
                );
                true
            }

            _ => {
                self.grammar.set_cat(target, left_child_id, right_child_id);
                false
            }
        }
    }

    // Takes a base node, a child node, and a parnt node, and modifies
    // the base node, attaching children as needed.  Returns true if
    // the operation was an optimization.
    //
    fn set_optimized_red(&mut self, target: NodeId, child_id: NodeId, source_id: NodeId) -> bool {
        let child = self.grammar[child_id].clone();

        match child.data {
            Parser::Emp => {
                self.grammar.set_emp(target);
                true
            }

            Parser::Eps(ref eps) => {
                if let Parser::Red(ref func) = self.grammar[source_id].data.clone() {
                    let t = func(self, eps);
                    self.grammar.set_eps(target, &Rc::new(t));
                    true
                } else {
                    unreachable!()
                }
            }

            Parser::Red(ref gunc) => {
                if let Parser::Red(ref func) = self.grammar[source_id].data.clone() {
                    let f = func.clone();
                    let g = gunc.clone();
                    self.grammar.set_red(
                        target,
                        child.left,
                        Rc::new(move |grammar, ts| {
                            let t = g(grammar, ts);
                            f(grammar, &t)
                        }),
                    );
                    true
                } else {
                    unreachable!()
                }
            }

            _ => {
                if let Parser::Red(ref func) = self.grammar[source_id].data.clone() {
                    self.grammar.set_red(target, child_id, func.clone());
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
    fn derive(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        {
            if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
                return *cached_node;
            };
        };

        let node = &self.grammar[nodeid].clone();

        let next_derivative = {
            match node.data {
                Parser::Emp => self.grammar.empty,

                Parser::Eps(_) => self.grammar.empty,

                Parser::Tok(ref t) => {
                    if *t == *token {
                        self.make_eps_from_token(token)
                    } else {
                        self.grammar.empty
                    }
                }

                Parser::Alt | Parser::Cat | Parser::Red(_) => self.make_ukn(),

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
                            permute(&ts1, &ts2)
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

        let mut node = self.grammar[nodeid].clone();
        let mut node_pair = NodePair(nodeid, &mut node);
        self.cached_nullable(&mut node_pair, None, &Nullable::Unvisited)
    }

    fn cached_nullable(
        &mut self,
        nodepair: &mut NodePair<T, U>,
        parent: Option<NodeId>,
        status: &Nullable,
    ) -> bool {
        let nullable = &self.nulls[&nodepair.0].clone();
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
                self.nulls.insert(nodepair.0, Nullable::InProgress);
                if self.compute_notify_nullable(nodepair, status) {
                    return true;
                }

                if let Some(parent) = parent {
                    let listeners = &mut self.listeners.entry(parent).or_insert(vec![]);
                    listeners.push(nodepair.0);
                }
                false
            }
        }
    }

    fn compute_notify_nullable(&mut self, nodepair: &mut NodePair<T, U>, status: &Nullable) -> bool {
        if !self.base_nullable(nodepair, status) {
            return false;
        }

        self.nulls.insert(nodepair.0, Nullable::Accept);
        if let Some((_, listeners)) = self.listeners.remove_entry(&nodepair.0) {
            for childnode in listeners {
                let mut node = self.grammar[childnode].clone();
                let mut newnp = NodePair(childnode.clone(), &mut node);
                self.compute_notify_nullable(&mut newnp, &Nullable::Accept);
            }
        }
        self.grammar[nodepair.0] = nodepair.1.clone();
        true
    }

    fn base_nullable(&mut self, nodepair: &mut NodePair<T, U>, status: &Nullable) -> bool {
        let nodeid = nodepair.0;
        match &nodepair.1.data {
            Parser::Emp => false,

            Parser::Eps(_) => true,

            Parser::Tok(_) => false,

            Parser::Alt => {
                let mut leftnode = self.grammar[nodepair.1.left].clone();
                let mut rightnode = self.grammar[nodepair.1.right].clone();
                self.cached_nullable(&mut NodePair(nodepair.1.left, &mut leftnode), Some(nodeid), status)
                    || self.cached_nullable(&mut NodePair(nodepair.1.right, &mut rightnode), Some(nodeid), status)
            }

            Parser::Cat => {
                let mut leftnode = self.grammar[nodepair.1.left].clone();
                let mut rightnode = self.grammar[nodepair.1.right].clone();
                self.cached_nullable(&mut NodePair(nodepair.1.left, &mut leftnode), Some(nodeid), status)
                    && self.cached_nullable(&mut NodePair(nodepair.1.right, &mut rightnode), Some(nodeid), status)
            }

            Parser::Red(_) => {
                let mut leftnode = self.grammar[nodepair.1.left].clone();
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

    pub fn parse_tree_inner(&mut self, memo: &mut ExtractionType<U>, nodeid: NodeId) -> Forest<U> {
        if let Some(cached_result) = memo.get(&nodeid) {
            return cached_result.clone();
        };

        if !self.nullable(nodeid) {
            return Forest::new();
        };

        let node = &self.grammar[nodeid].clone();

        match &node.data {
            Parser::Emp => Forest::new(),

            Parser::Tok(_) => Forest::new(),

            Parser::Eps(ref eps) => (**eps).clone(),

            Parser::Alt => {
                let p1 = self.parse_tree_inner(memo, node.left);
                union(&p1, &self.parse_tree_inner(memo, node.right))
            }

            Parser::Cat => {
                let p1 = self.parse_tree_inner(memo, node.left);
                permute(&p1, &self.parse_tree_inner(memo, node.right))
            }

            Parser::Red(ref red) => {
                let t_tree = self.parse_tree_inner(memo, node.left);
                (*red)(self, &t_tree)
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
    pub fn parse<I>(&mut self, items: &mut I) -> Option<Forest<U>>
    where
        I: Iterator<Item = T>,
    {
        let mut nodeid = self.grammar.start;
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
                    let nl = &self.grammar[np].data.clone();
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

impl<T: Siaa, U: Riaa<T>> ParseTreeExtractor<T, U> for Deriver<T, U>
    where U: std::convert::From<T>
{
    fn parse_tree(&mut self, start: NodeId) -> Forest<U> {
        let mut memo: ExtractionType<U> = HashMap::new();
        self.parse_tree_inner(&mut memo, start)
    }
}
