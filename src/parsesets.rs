use consy::Cell;
use indexmap::IndexSet;
use siaa::Siaa;

pub type NodeId = usize;

// A Parse Tree is just a cons tree with immutable interiors.  Those
// interiors can be replaced, however, which is fine for our purposes.
pub type Tree<U> = Cell<U>;

// Our system returns Parse _Forests_, sets of parse trees permuted by
// the different variants found within a single parse.  Using the
// IndexSet allows us to have two variants of the parse() function:
// one in which the first-one-wins rule (the PEG semantic) is in
// effect, and one in which the longest-one-wins (the POSIX semantic)
// is in effect.

#[derive(Debug)]
pub struct Forest<U: Siaa>(pub IndexSet<Tree<U>>);

// Unfortunately, there is at least one rule that requires a parse tree
// extractor be present, and so the grammar must be available.  Annoying
// as hell, but there's not a lot I can do.
pub trait ParseTreeExtractor<T: Siaa, U: Siaa> {
    fn parse_tree(&mut self, start: NodeId) -> Forest<U>;
}

pub type RedFn<T, U> = Fn(&mut ParseTreeExtractor<T, U>, &Forest<U>) -> Forest<U>;

impl<U: Siaa> Forest<U> {
    pub fn new() -> Forest<U> {
        Forest(IndexSet::new())
    }

    // Move semantics with this, almost always.
    pub fn with(pt: Tree<U>) -> Forest<U> {
        let mut ps = Forest::new();
        ps.0.insert(pt);
        ps
    }

    pub fn insert(&mut self, pt: Tree<U>) {
        self.0.insert(pt);
    }
}

impl<U: Siaa> Default for Forest<U> {
    fn default() -> Forest<U> {
        Forest::new()
    }
}

impl<U: Siaa> Clone for Forest<U> {
    fn clone(&self) -> Forest<U> {
        Forest(self.0.clone())
    }
}
