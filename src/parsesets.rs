use hashbrown::HashSet;

use consy::cons;

use consy::Cell;
use std::rc::Rc;
pub type NodeId = usize;

// This is basically a simple cons list, meant to be constructed and
// cloned ad-hoc for the purpose of generating the final parse tree.
// I'm starting to think that the external reductions will have to be
// done with another Lit() type, for extracting the trees
// post-processed.

pub type ParseTree = Cell<char>;

#[derive(Debug)]
pub struct ParseSet(pub HashSet<ParseTree>);

pub trait ParseTreeExtractor {
    fn parse_tree(&mut self, start: NodeId) -> ParseSet;
}

pub type RedFn = Fn(&mut ParseTreeExtractor, &ParseSet) -> ParseSet;

#[macro_export]
macro_rules! parseset {
    ( $( $x:expr ),* ) => {{
        let mut h = HashSet::new();
        $( h.insert($x); )*
        ParseSet(h)
    }};
}

impl ParseSet {
    pub fn new() -> ParseSet {
        ParseSet(HashSet::new())
    }

    // Move semantics with this, almost always.
    pub fn with(pt: ParseTree) -> ParseSet {
        let mut ps = ParseSet::new();
        ps.0.insert(pt);
        ps
    }

    pub fn insert(&mut self, pt: ParseTree) {
        self.0.insert(pt);
    }

    pub fn union(&self, other: &ParseSet) -> ParseSet {
        ParseSet(other.0.union(&self.0).cloned().collect())
    }

    pub fn permute(&self, other: &ParseSet) -> ParseSet {
        let mut ret = ParseSet::new();
        for t1 in &self.0 {
            for t2 in &other.0 {
                ret.0.insert(cons!(t1.clone(), t2.clone()));
            }
        }
        ret
    }

    pub fn rebalance_after_seq(&self) -> ParseSet {
        // Optimization: (p1 ◦ p2) ◦ p3 ⇒ (p1 ◦ (p2 ◦ p3)) → λu. {((t1, t2), t3) | (t1,(t2, t3)) ∈ u}
        // See grammar::Grammer::set_optimized_cat_left for details.
        let mut ret = ParseSet::new();
        for t1 in &self.0 {
            ret.insert(if t1.pairp() && t1.cdr().unwrap_or(&Cell::Nil).pairp() {
                cons!(
                    cons!(t1.car().unwrap().clone(), t1.cadr().unwrap().clone()),
                    t1.cddr().unwrap().clone()
                )
            } else {
                // Because this is a reconstruction after an
                // optimization, the above structure must be
                // correct.  If it's not, we've got bigger
                // problems.
                unreachable!()
            })
        }
        ret
    }

    // [red-tag (make-red-node (make-seq-node (node-child1 left) right)
    //     (let ([f (node-child2 left)])
    //         (lambda (ts) (for*/list ([t ts][t+ (f (list (car t)))]) (cons t+ (cdr t))))))]

    // Optimization: (p1 → f) ◦ p2 ⇒ (p1 ◦ p2) → λu. {(f({t1}) , t2) | (t1, t2) ∈ u}
    // See grammar::Grammer::set_optimized_cat_left for details.
    //
    pub fn run_after_floated_reduction(&self, grammar: &mut ParseTreeExtractor, func: &Rc<RedFn>) -> ParseSet {
        let mut ret = ParseSet::new();
        for t1 in &self.0 {
            for t2 in func(grammar, &parseset!(t1.car().unwrap().clone())).0 {
                ret.insert(cons!(t2, t1.cdr().unwrap().clone()))
            }
        }
        ret
    }
}

impl Default for ParseSet {
    fn default() -> ParseSet {
        ParseSet::new()
    }
}

impl Clone for ParseSet {
    fn clone(&self) -> ParseSet {
        ParseSet(self.0.clone())
    }
}
