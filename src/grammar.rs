use std::collections::HashMap;
use std::iter::Peekable;
use types::{Parser, Siaa};
use arena::{Arena, Node, NodeId};

pub struct Grammar<T: Siaa> {
    pub arena: Arena<Parser<T>>,
    pub memo: HashMap<(NodeId, T), NodeId>,
    pub empty: NodeId,
    pub epsilon: NodeId,
}

macro_rules! add_node {
    ( $source:ident, $par:ident, $lhs:expr, $rhs:expr ) => {
        {
            let newparser = $source.add($par);
            $source[newparser].left = $lhs;
            $source[newparser].right = $rhs;
            newparser
        }
    };
}


impl<T: Siaa> Grammar<T> {
    fn get_next_derivative(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        let node = &self.arena[nodeid].clone();

        match node.data {
            // Dc(∅) = ∅
            Parser::Emp => self.empty,

            // Dc(ε) = ∅
            Parser::Eps(_) => self.empty,

            // Dc(c) = ε if c = c'
            // Dc(c') = ∅ if c ≠ c'
            Parser::Tok(ref t) => {
                if *t == *token {
                    self.epsilon
                } else {
                    self.empty
                }
            }

            // Dc(re1 | re2) = Dc(re1) | Dc(re2)
            Parser::Alt => {
                let alt = self.arena.add(Parser::Alt);
                self.arena[alt].left = self.derive(node.left, &token);
                self.arena[alt].right = self.derive(node.right, &token);
                alt
            }

            // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
            // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
            Parser::Cat => {
                let dcl = self.derive(node.left, &token);
                let lhs = self.arena.add(Parser::Cat);
                self.arena[lhs].left = dcl;
                self.arena[lhs].right = node.right;

                if self.nullable(node.left) {
                    let dcr = self.derive(node.right, &token);
                    let nca = self.arena.add(Parser::Alt);
                    self.arena[nca].left = lhs;
                    self.arena[nca].right = dcr;
                    nca
                } else {
                    lhs
                }
            }

            // Dc(re*) = Dc(re) re*
            Parser::Rep => {
                let rep = self.arena.add(Parser::Cat);
                let car = self.derive(node.left, &token);
                self.arena[rep].left = car;
                self.arena[rep].right = nodeid;
                rep
            }
        }
    }

    fn derive(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        // If we have already seen this node, go get it and process it.
        
        if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
            return *cached_node;
        };

        let next_derivative = self.get_next_derivative(nodeid, token);
        self.memo.insert((nodeid, token.clone()), next_derivative);
        next_derivative
    }

    fn nullable(&self, nodeid: NodeId) -> bool {
        let node = &self.arena[nodeid];

        match node.data {
            Parser::Emp => false,
            Parser::Eps(_) => true,
            Parser::Tok(_) => false,
            Parser::Alt => self.nullable(node.left) || self.nullable(node.right),
            Parser::Cat => self.nullable(node.left) && self.nullable(node.right),
            Parser::Rep => true,
        }
    }

    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut current_node = start;
        let mut items = items.peekable();
        loop {
            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => break self.nullable(current_node),
                
                Some(ref c) => {
                    let np = self.derive(current_node, c);
                    let nl = &self.arena[np].data;
                    match nl {
                        Parser::Emp => break false,
                        Parser::Eps(_) => match items.peek() {
                            Some(_) => break false,
                            None => break true,
                        },
                        // Essentially, for all other possibilities, we
                        // just need to recurse across our nodes until
                        // we hit Empty or Epsilon, and then we're
                        // done.
                        _ => { current_node = np; }
                    }
                }
            }
        }
    }
}
