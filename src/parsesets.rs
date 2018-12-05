use std::collections::HashSet;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum ParseTree {
    Nil,
    Lit(char),
    Pair(Box<ParseTree>, Box<ParseTree>),
}

pub struct ParseSet(pub HashSet<ParseTree>);

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
                ret.0.insert(ParseTree::Pair(Box::new(t1.clone()), Box::new(t2.clone())));
            }
        }
        ret
    }
}

impl Clone for ParseSet {
    fn clone(&self) -> ParseSet {
        ParseSet(self.0.clone())
    }
}

