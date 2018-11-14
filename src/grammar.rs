#![warn(clippy::shadow_reuse, clippy::shadow_same, clippy::shadow_unrelated)]
#![allow(clippy::new_without_default_derive, clippy::redundant_field_names)]
use std::collections::HashMap;
use types::{Alt, Cat, Eps, Laz, Node, NodeId, Nullable, Siaa, TokenClass};

pub struct NodePair<'a, T: Siaa>(NodeId, &'a mut Node<T>);

pub struct Grammar<T: Siaa> {
    pub language: Vec<Node<T>>,
    pub memo: HashMap<(NodeId, T), NodeId>,
    pub listeners: HashMap<NodeId, Vec<NodeId>>,
    pub start: NodeId,
    pub empty: NodeId
}

impl<T: Siaa + 'static> Grammar<T> {
    fn len(&self) -> NodeId {
        self.language.len() - 1
    }
    
    fn push(&mut self, node: Node<T>) -> NodeId {
        self.language.push(node);
        self.len()
    }
    
    //  ___               
    // | __|__ _ _ __ ___ 
    // | _/ _ \ '_/ _/ -_)
    // |_|\___/_| \__\___|
    //                    
    fn force(&mut self, this: NodeId, parent: NodeId, token: &T) -> NodeId {
        let replacement = {
            let node = self.language[parent].clone();
            // println!("Force,    Node: {:-2?} Token: {:-2?} {:?}", this, token, node);

            match node {
                Node::Alt(ref alt) => {
                    let left = self.derive(alt.left, token);
                    let right = self.derive(alt.right, token);
                    Node::Alt(Alt::new(left, right))
                }

                Node::Cat(ref cat) => {
                    let derived_left = self.derive(cat.left, token);
                    let lhs = Node::Cat(Cat::new(derived_left, cat.right));
                    if self.nullable(cat.left) {
                        let derived_right = self.derive(cat.right, token);
                        Node::Alt(Alt::new(self.push(lhs), derived_right))
                    } else {
                        lhs
                    }
                }

                Node::Rep(ref rep) => {
                    let child = self.derive(rep.child, &token);
                    Node::Cat(Cat::new(child, parent))
                }

                _ => panic!("Force called on unambiguous node."),
            }
        };

        self.language[this] = replacement;
        this
    }

    //  ___          _         
    // |   \ ___ _ _(_)_ _____ 
    // | |) / -_) '_| \ V / -_)
    // |___/\___|_| |_|\_/\___|
    //                         
    fn get_next_derivative(&mut self, node: &Node<T>, token: &T, this: NodeId) -> NodeId {
        // println!("Derive,   Node: {:-2?} Token: {:?} {:?}", this, token, node);
        match node {
            // Dc(∅) = ∅
            Node::Emp(_) => self.empty,

            // Dc(ε) = ∅
            Node::Eps(_) => self.empty,

            // Dc(c) = ε if c = c'
            // Dc(c') = ∅ if c ≠ c'
            Node::Tok(ref tok) => {
                if tok.predicate.has(&token) {
                    self.push(Node::Eps(Eps::new(token.clone())))
                } else {
                    self.empty
                }
            }

            // Dc(re1 | re2) = Dc(re1) | Dc(re2)
            Node::Alt(_) => self.push(Node::Laz(Laz::new(this, token.clone()))),

            // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
            // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
            Node::Cat(_) => self.push(Node::Laz(Laz::new(this, token.clone()))),

            // Dc(re*) = Dc(re) re*
            Node::Rep(_) => self.push(Node::Laz(Laz::new(this, token.clone()))),

            Node::Laz(ref laz) => {
                let nnid = self.force(this, laz.parent, &laz.content);
                self.derive(nnid, token)
            }
        }
    }

    fn derive(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        // If we have already seen this node, go get it and process it.

        if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
            return *cached_node;
        };

        let node = &mut self.language[nodeid].clone();
        let next_derivative = self.get_next_derivative(node, token, nodeid);
        self.memo.insert((nodeid, token.clone()), next_derivative);
        next_derivative
    }

    //  _  _      _ _      _    _ _ _ _
    // | \| |_  _| | |__ _| |__(_) (_) |_ _  _
    // | .` | || | | / _` | '_ \ | | |  _| || |
    // |_|\_|\_,_|_|_\__,_|_.__/_|_|_|\__|\_, |
    //                                    |__/

    // Primary interface to the nullability function.
    fn nullable(&mut self, nodeid: NodeId) -> bool {

        // Strategy: Get a *copy* of the node, passing around a
        // reference kept alive during nullability testing. Always
        // associate a node with its ID.  When mutating the node,
        // mutate the local node then write a copy of it back to the
        // language.
        let mut node = self.language[nodeid].clone();
        let mut node_pair = NodePair(nodeid, &mut node);
        self.cached_nullable(&mut node_pair, None, &Nullable::Unvisited)
    }

    fn cached_nullable(
        &mut self,
        nodepair: &mut NodePair<T>,
        parent: Option<NodeId>,
        status: &Nullable,
    ) -> bool {
        use self::Nullable::*;

        let nullable = nodepair.1.is_nullable();
        match nullable {
            Accept => true,
            Reject => false,
            InProgress => {
                if let Some(parent) = parent {
                    let listeners = self.listeners.entry(parent).or_insert(vec![]);
                    listeners.push(nodepair.0);
                } else {
                    panic!("InProgress called without parent. #CANTHAPPEN");
                }
                false
            }
            Unvisited => {
                nodepair.1.set_nullable(Nullable::InProgress);
                if self.compute_notify_nullable(nodepair, status) {
                    true
                } else {
                    if let Some(parent) = parent {
                        let listeners = self.listeners.entry(parent).or_insert(vec![]);
                        listeners.push(nodepair.0);
                    }
                    false
                }
            }
        }
    }

    fn set_nullable_and_mark(
        &mut self,
        nodepair: &mut NodePair<T>,
        status: &Nullable,
    ) -> bool {
        nodepair.1.set_nullable(status.clone());
        if let Some((_, listeners)) = self.listeners.remove_entry(&nodepair.0) {
            for childnode in listeners {
                let mut node = self.language[childnode].clone();
                let mut newnp = NodePair(childnode.clone(), &mut node);
                self.compute_notify_nullable(&mut newnp, status);
            }
        }

        // Big dangerous mutation here.
        self.language[nodepair.0] = nodepair.1.clone();
        true
    }

    fn compute_notify_nullable(
        &mut self,
        nodepair: &mut NodePair<T>,
        status: &Nullable,
    ) -> bool {
        if self.might_nullable(nodepair, status) {
            self.set_nullable_and_mark(nodepair, &Nullable::Accept)
        } else {
            false
        }
    }

    fn might_nullable(&mut self, nodepair: &mut NodePair<T>, status: &Nullable) -> bool {
        use self::Node::*;

        let nodeid = nodepair.0;
        match nodepair.1 {
            Emp(_) => false,
            Eps(_) => true,
            Tok(_) => false,
            Alt(ref alt) => {
                let mut leftnode = self.language[alt.left].clone();
                let mut rightnode = self.language[alt.right].clone();
                self.cached_nullable(&mut NodePair(alt.left, &mut leftnode), Some(nodeid), status) ||
                    self.cached_nullable(&mut NodePair(alt.right, &mut rightnode), Some(nodeid), status)
            }
            Cat(ref cat) => {
                let mut leftnode = self.language[cat.left].clone();
                let mut rightnode = self.language[cat.right].clone();
                self.cached_nullable(&mut NodePair(cat.left, &mut leftnode), Some(nodeid), status)
                    && self.cached_nullable(&mut NodePair(cat.right, &mut rightnode), Some(nodeid), status)
            }
            // Red(ref red) => { self.cached_nullable(red.child, Some(nodeid), status) }
            Rep(_) => true,
            Laz(ref laz) => {
                let nnid = self.force(nodeid, laz.parent, &laz.content);
                let mut node = self.language[nnid].clone();
                self.cached_nullable(&mut NodePair(nnid, &mut node), None, status)
            }
        }
    }

    //  ___                  
    // | _ \__ _ _ _ ___ ___ 
    // |  _/ _` | '_(_-</ -_)
    // |_| \__,_|_| /__/\___|
    //                       
    pub fn parse<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut items = items.peekable();
        let mut current_node = self.start;
        loop {
            let token = items.next();
            // println!("Passing: {:?}", token);
            // println!("Language: {:?}", &self.language);
            use self::Node::*;
            match token {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => break self.nullable(current_node),

                Some(ref c) => {
                    let np = self.derive(current_node, c);
                    let nl = &self.language[np];
                    match nl {
                        Emp(_) => break false,
                        Eps(_) => match items.peek() {
                            Some(_) => break false,
                            None => break true,
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
    
