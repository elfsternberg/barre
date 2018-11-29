use arena::{Arena, Node, NodeId};
use std::collections::{HashMap, HashSet};
use types::{Parser, Siaa};
use std::hash::Hash;
use render::render;

pub struct Grammar<T: Siaa> {
    pub arena: Arena<Parser<T>>,
    pub memo: HashMap<(NodeId, T), NodeId>,
    pub empty: NodeId,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum ParseTree<T>
    where T: Hash + Eq + PartialEq
{
    Lit(T),
    Pair(Box<ParseTree<T>>, Box<ParseTree<T>>)
}

// Deliberate mechanism to build things in the correct order, so that
// the compiler's habit of building temporaries is circumvented and
// the borrow rules work correctly.  Also: A lot easier.

macro_rules! add_node {
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
        if $condition { $_true } else { $_false }
    };
}

// Using a '+' instead of a '*' in the second expression allows this
// macro to build code that can be used immutably; without it, the
// compiler complains that building an empty hashset does not require
// an initial mutable definition.  Annoying, but formally correct.

#[macro_export]
macro_rules! hashset {
    () => {
        std::collections::HashSet::new()
    };

    ( $( $x:expr ),+ ) => {
        {
            let mut hset = hashset!();
            $(hset.insert($x);)*
            hset
        }
    };
}

type ExtractionType<T> = HashMap<(NodeId), HashSet<ParseTree<T>>>;

impl<T: Siaa> Grammar<T>
{
    fn add(&mut self, parser: Parser<T>) -> NodeId {
        self.arena.add(parser)
    }

    fn force(&mut self, nodeid: NodeId, parent: NodeId, token: &T) -> NodeId {
        let node = &self.arena[parent].clone();

        let replacement = {
            match node.data {
                Parser::Alt => Node::new(Parser::Alt),
                Parser::Cat => Node::new(Parser::Alt),
                // Parser::Rep => Node::new(Parser::Cat),
                _ => panic!("Force called on unambiguous node."),
            }
        };

        self.arena[nodeid] = replacement;

        match node.data {
            Parser::Alt => {
                let l = self.derive(node.left, &token);
                let r = self.derive(node.right, &token);
                if l == self.empty {
                    self.arena[nodeid] = self.arena[r].clone();
                }
                else if r == self.empty {
                    self.arena[nodeid] = self.arena[l].clone();
                }
                else {
                    self.arena[nodeid].left = self.derive(node.left, &token);
                    self.arena[nodeid].right = self.derive(node.right, &token);
                }
            },

            // Parser::Rep => {
            //     self.arena[nodeid].left = self.derive(node.left, &token);
            //     self.arena[nodeid].right = parent;
            // },

            Parser::Cat => {
                let l = self.derive(node.left, &token);
                let r = self.derive(node.right, &token);
                if l == self.empty || r == self.empty {
                    return self.empty
                }

                self.arena[nodeid].left = add_node!(
                    self, Parser::Cat, add_node!(self, Parser::Del, node.left), r);
                self.arena[nodeid].right = add_node!(self, Parser::Cat, l, node.right);
            },
            _ => panic!("Force called on a non-lazy node."),
        };

        nodeid
    }

    fn derive(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        // If we have already seen this node, go get it and process it.
        {
            if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
                return *cached_node;
            };
        };

        let next_derivative = {
            let node = &self.arena[nodeid].clone();

            match node.data {
                // Dc(∅) = ∅
                Parser::Emp => self.empty,

                // Dc(ε) = ∅
                Parser::Eps(_) => self.empty,

                Parser::Del => self.empty,

                // Dc(c) = ε if c = c'
                // Dc(c') = ∅ if c ≠ c'
                Parser::Tok(ref t) => {
                    iff!(*t == *token, self.add(Parser::Eps(token.clone())), self.empty)
                }

                Parser::Alt | Parser::Cat =>
                    add_node!(self, Parser::Laz(token.clone()), nodeid),

                Parser::Laz(ref c) => {
                    let nnid = self.force(nodeid, node.left, c);
                    self.derive(nnid, token)
                }

                Parser::Unk => {
                    panic!("Unknown node in derive operation. CANTHAPPEN.");
                }
            }
        };

        self.memo.insert((nodeid, token.clone()), next_derivative);
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
            Parser::Laz(ref c) => {
                let fnid = self.force(nodeid, node.left, c);
                self.nullable(fnid)
            }
            Parser::Unk => {
                panic!("Unknown node in nullable operation. CANTHAPPEN.");
            }
        }
    }

    pub fn parse_tree_inner(&mut self, memo: &mut ExtractionType<T>, nodeid: NodeId) -> HashSet<ParseTree<T>> {
        if let Some(cached_result) = memo.get(&nodeid) {
            return cached_result.clone();
        };

        if !self.nullable(nodeid) {
            return hashset!();
        };

        let node = &self.arena[nodeid].clone();

        match &node.data {
            Parser::Emp => hashset!(),
            Parser::Tok(_) => hashset!(),
            Parser::Eps(ref c) => hashset!(ParseTree::Lit(c.clone())),
            Parser::Del => {
                self.parse_tree_inner(memo, node.left)
            },
                
            Parser::Alt => {
                let p1 = self.parse_tree_inner(memo, node.left);
                p1.union(&self.parse_tree_inner(memo, node.right)).into_iter().cloned().collect()
            }
            Parser::Cat => {
                let mut ret = HashSet::new();
                let p2 = self.parse_tree_inner(memo, node.right);
                for t1 in self.parse_tree_inner(memo, node.left) {
                    for t2 in &p2 {
                        ret.insert(ParseTree::Pair(Box::new(t1.clone()), Box::new(t2.clone())));
                    }
                }
                ret

            }
            Parser::Laz(ref c) => {
                let fnid = self.force(nodeid, node.left, c);
                self.parse_tree_inner(memo, fnid)
            }
            Parser::Unk => {
                panic!("Unknown node in parsetree operation. CANTHAPPEN");
            }
        }
    }

    pub fn parse_tree(&mut self, start: NodeId) -> HashSet<ParseTree<T>> {
        let mut memo: ExtractionType<T> = HashMap::new();
        self.parse_tree_inner(&mut memo, start)
    }

    pub fn parse<I>(&mut self, items: &mut I, start: NodeId) -> Option<HashSet<ParseTree<T>>>
    where
        I: Iterator<Item = T>,
    {
        let mut current_node = start;
        let mut items = items.peekable();
        let mut count = 0;
        loop {
            render(&self.arena, current_node, &format!("output-{:?}.dot", count));
            count = count + 1;

            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => {
                    break if self.nullable(current_node) {
                        render(&self.arena, current_node, &format!("output-{:?}.dot", count));
                        Some(self.parse_tree(current_node))
                    } else {
                        None
                    }
                }

                Some(ref c) => {
                    let np = self.derive(current_node, c);
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
                            current_node = np;
                        }
                    }
                }
            }
        }
    }
}

