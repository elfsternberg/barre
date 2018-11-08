use std::collections::HashMap;

use language::Language;
use builder::{init_barre_vec, language_to_vec_rep};
use types::{Siaa, Node, NodeId};

pub struct Barre<T:Siaa>
{
    language: Vec<Node<T>>,
    memo: HashMap<(NodeId, T), NodeId>,
    start: NodeId,
    empty: NodeId,
    epsilon: NodeId,
}

impl<T: Siaa> Barre<T>
{
    pub fn new() -> Barre<T> {
        // Currently, this recognizer recognizes no strings of tokens
        Barre::<T> {
            language: init_barre_vec(),
            memo: HashMap::new(),
            start: 1,
            epsilon: 0,
            empty: 1,
        }
    }

    pub fn from_language(lang: &Language<T>) -> Barre<T>
    {

        let new_representation = language_to_vec_rep(lang);
        let start = new_representation.len() - 1;

        Barre::<T> {
            // original: new_representation.clone(),
            language: new_representation,
            memo: HashMap::new(),
            start: start,
            epsilon: 0,
            empty: 1,
        }
    }

    fn len(&self) -> NodeId {
        self.language.len() - 1
    }

    fn push(&mut self, node: Node<T>) -> NodeId {
        self.language.push(node);
        self.len()
    }

    fn get_next_derivative(&mut self, node: &Node<T>, token: &T, this: NodeId) -> NodeId {
        use self::Node::*;
        match node {
            // Dc(∅) = ∅
            Emp => self.empty,

            // Dc(ε) = ∅
            Eps(_) => self.empty,

            // Dc(c) = ε if c = c'
            // Dc(c') = ∅ if c ≠ c'
            Tok(ref t) => {
                if *t == *token {
                    self.epsilon
                } else {
                    self.empty
                }
            }

            // Dc(re1 | re2) = Dc(re1) | Dc(re2)
            Alt(car, cdr) => {
                let ncar = self.derive(*car, &token);
                let ncdr = self.derive(*cdr, &token);
                self.push(Node::Alt(ncar, ncdr))
            }

            // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
            // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
            Cat(car, cdr) => {
                let dcl = self.derive(*car, &token);
                let lhs = self.push(Node::Cat(dcl, *cdr));
                if self.nullable(*car) {
                    let dcr = self.derive(*cdr, &token);
                    self.push(Node::Alt(lhs, dcr))
                } else {
                    lhs
                }
            }

            // Dc(re*) = Dc(re) re*
            Rep(nest) => {
                let car = self.derive(*nest, &token);
                self.push(Node::Cat(car, this))
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

    fn nullable(&self, node: NodeId) -> bool {
        let node = &self.language[node];

        use self::Node::*;
        match node {
            Emp => false,
            Eps(_) => true,
            Tok(_) => false,
            Alt(car, cdr) => self.nullable(*car) || self.nullable(*cdr),
            Cat(car, cdr) => self.nullable(*car) && self.nullable(*cdr),
            Rep(_) => true,
        }
    }

    pub fn match_re<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut items = items.peekable();
        let mut current_node = self.start;
        loop {
            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => break self.nullable(current_node),
                
                Some(ref c) => {
                    let np = self.derive(current_node, c);
                    let nl = &self.language[np];
                    match nl {
                        Node::Emp => break false,
                        Node::Eps(_) => match items.peek() {
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

#[cfg(test)]
mod tests {

    use super::Barre;
    use language::{cat, alt, rep, tok};

    macro_rules! mkpair {
        ($(($l:expr, $r:expr)),*) => {
            vec![$(($l, $r)),*]
            .into_iter()
            .map(|pair| { (String::from(pair.0), pair.1) });
        }
    }

    macro_rules! testpat {
        ($pt:ident; [$(($l:expr, $r:expr)),*]) => {
            {
                let matches = mkpair![$(($l, $r)),*];
                for pair in matches {
                    println!("{:?} {:?}", &pair.0, &pair.1);
                    assert!($pt.match_re(&mut pair.0.chars()) == pair.1);
                }
            }
        }
    }

    #[test]
    fn just_a_token() {
        let lang = tok('a');
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("a", true), ("", false), ("b", false),
            ("aab", false), ("aba", false)
        ]);
    }

    #[test]
    fn just_a_cat() {
        let lang = cat(tok('a'), tok('b'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("ab", true), ("", false), ("b", false),
            ("aab", false), ("aba", false), ("a", false)
        ]);

    }

    #[test]
    fn mildly_complex_macro_pattern() {
        let lang = alt!(
            cat!(tok('f'), tok('o'), tok('o')),
            cat!(tok('b'), tok('a'), tok('r')),
            cat!(tok('b'), tok('a'), tok('z'))
        );
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("foo", true), ("bar", true), ("baz", true),
            ("far", false), ("boo", false), ("ba", false),
            ("foobar", false), ("", false)
        ]);
    }

    #[test]
    fn slightly_more_complex_untyped_macro() {
        // /AB(CC|DDDD)E*F/
        let lang = cat!(tok('a'), tok('b'),
                        alt!(cat!(tok('c'), tok('c')),
                             cat!(tok('d'), tok('d'), tok('d'), tok('d'))),
                        rep(tok('e')), tok('f'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("abccf", true), ("abccef", true), ("abcceeeeeeef", true), ("abddddf", true),
            ("abddddef", true), ("abddddeeeeeef", true), ("ab", false), ("abcef", false),
            ("abcdef", false), ("abcceff", false), ("", false), ("abcddf", false)

        ]);
    }
}
