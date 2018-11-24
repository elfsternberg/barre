use std::collections::HashMap;
use types::{Parser, Siaa};
use arena::{Arena, Node, NodeId};

pub struct Grammar<T: Siaa> {
    pub arena: Arena<Parser<T>>,
    pub memo: HashMap<(NodeId, T), NodeId>,
    pub empty: NodeId,
    pub epsilon: NodeId,
}

macro_rules! add_node {
    ( $source:expr, $par:expr, $lhs:expr ) => {
        {
            let newparser = $source.add($par);
            $source[newparser].left = $lhs;
            newparser
        }
    };

    ( $source:expr, $par:expr, $lhs:expr, $rhs:expr ) => {
        {
            let newparser = $source.add($par);
            let car = $lhs;
            let cdr = $rhs;
            $source[newparser].left = car;
            $source[newparser].right = cdr;
            newparser
        }
    };
}


impl<T: Siaa> Grammar<T> {
    fn force(&mut self, nodeid: NodeId, parent: NodeId, token: &T) -> NodeId {
        let node = &self.arena[parent].clone();

        let replacement = {
            match node.data {
                Parser::Alt => Node::new(Parser::Alt),
                Parser::Cat => {
                    Node::new(if self.nullable(node.left) {
                        Parser::Alt
                    } else {
                        Parser::Cat
                    })
                },
                Parser::Rep => Node::new(Parser::Cat),

                _ => panic!("Force called on unambiguous node."),
            }
        };
        
        self.arena[nodeid] = replacement;

        match node.data {
            Parser::Alt => {
                self.arena[nodeid].left = self.derive(node.left, &token);
                self.arena[nodeid].right = self.derive(node.right, &token);
            },

            Parser::Rep => {
                self.arena[nodeid].left = self.derive(node.left, &token);
                self.arena[nodeid].right = parent;
            },

            Parser::Cat => {
                if self.nullable(node.left) {
                    self.arena[nodeid].left = add_node!(self.arena, Parser::Cat,
                                                        self.derive(node.left, &token), node.right);
                    self.arena[nodeid].right = self.derive(node.right, &token);
                } else {
                    self.arena[nodeid].left = self.derive(node.left, &token);
                    self.arena[nodeid].right = node.right;
                }
            },

            _ => panic!("Force called on unambiguous node."),
        };

        nodeid
    }
                    
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
            Parser::Alt | Parser::Cat | Parser::Rep => {
                let newparser = self.arena.add(Parser::Laz(token.clone()));
                self.arena[newparser].left = nodeid;
                newparser
            }

            Parser::Laz(ref c) => {
                let nnid = self.force(nodeid, node.left, c);
                self.derive(nnid, token)
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

    fn nullable(&mut self, nodeid: NodeId) -> bool {
        let node = &self.arena[nodeid].clone();

        match node.data {
            Parser::Emp => false,
            Parser::Eps(_) => true,
            Parser::Tok(_) => false,
            Parser::Alt => self.nullable(node.left) || self.nullable(node.right),
            Parser::Cat => self.nullable(node.left) && self.nullable(node.right),
            Parser::Rep => true,
            Parser::Laz(ref c) => {
                let fnid = self.force(nodeid, node.left, c);
                self.nullable(fnid)
            }
        }
    }

    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut current_node = start;
        let mut items = items.peekable();
        loop {
            println!("{:?}", self.arena);
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
