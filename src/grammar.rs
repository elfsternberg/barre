use arena::{Arena, NodeId};
use render::render;
use std::collections::{HashMap, HashSet};
use types::Parser;
use parsesets::{ParseSet, ParseTree};

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

    fn get_lr(&self, nodeid: NodeId) -> (NodeId, NodeId) {
        let node = &self.arena[nodeid];
        (node.left, node.right)
    }

    fn derive_cat(&mut self, nodeid: NodeId, target: NodeId, token: &char) -> NodeId {
        let (node_left, node_right) = self.get_lr(nodeid);
        if self.nullable(node_left) {
            let l = self.derive(node_left, &token);
            self.arena[target].right = add_node!(self, Parser::Cat, l, node_right);
            let r = self.derive(node_right, &token);
            self.arena[target].left = add_node!(self, Parser::Cat, add_node!(self, Parser::Del, node_left), r);
            self.arena[target].data = Parser::Alt;
        } else {
            self.arena[target].left = self.derive(node_left, &token);
            self.arena[target].right = node_right;
            self.arena[target].data = Parser::Cat;
        }
        target
    }

    fn derive_alt(&mut self, nodeid: NodeId, target: NodeId, token: &char) -> NodeId {
        let (node_left, node_right) = self.get_lr(nodeid);
        let l = self.derive(node_left, &token);
        let r = self.derive(node_right, &token);
        if l == self.empty {
            self.arena[target] = self.arena[r].clone();
        } else if r == self.empty {
            self.arena[target] = self.arena[l].clone();
        } else {
            self.arena[target].left = self.derive(node_left, &token);
            self.arena[target].right = self.derive(node_right, &token);
            self.arena[target].data = Parser::Alt;
        }
        target
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
                Parser::Alt | Parser::Cat => self.add(Parser::Ukn),
                Parser::Ukn => {
                    panic!("Uknnown node in derive operation. CANTHAPPEN.");
                }
            }
        };

        self.memo.insert((nodeid, token.clone()), next_derivative);

        match node.data {
            Parser::Cat => self.derive_cat(nodeid, next_derivative, token),
            Parser::Alt => self.derive_alt(nodeid, next_derivative, token),
            _ => { next_derivative }
        }
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
            Parser::Ukn => {
                panic!("Uknnown node in parsetree operation. CANTHAPPEN");
            }
        }
    }

    pub fn parse_tree(&mut self, start: NodeId) -> HashSet<ParseTree> {
        let mut memo: ExtractionType = HashMap::new();
        self.parse_tree_inner(&mut memo, start).0    // Note trailing struct extraction.
    }

    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> Option<HashSet<ParseTree>>
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
