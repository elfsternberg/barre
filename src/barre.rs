use std::collections::HashMap;

type NodeId = usize;

use builder::Language;

#[derive(Copy, Clone, Debug)]
pub struct Node<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    car: NodeId,
    cdr: NodeId,
    derivative: Option<NodeId>,
    token: T
}

impl<T> Node<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    pub fn new(car: NodeId, cdr: NodeId, derivative: Option<NodeId>, token: T) -> Node<T> {
        Node { car: car, cdr: cdr, derivative: derivative, token: token }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Brzop<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    Empty,
    Epsilon,
    Token(Node<T>),
    Alt(Node<T>),
    Cat(Node<T>),
    Repeat(Node<T>),
}


pub struct Barre<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    // original: Vec<Brzop<T>>,
    language: Vec<Brzop<T>>,
    memo: HashMap<(NodeId, T), NodeId>,
    start: NodeId,
    empty: NodeId,
    epsilon: NodeId,
}

pub fn toknode<T>(token: T) -> Brzop<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    Brzop::Token(Node::new(0, 0, None, token))
}

pub fn altnode<T>(car: NodeId, cdr: NodeId, token: T) -> Brzop<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    Brzop::Alt(Node::new(car, cdr, None, token))
}

pub fn catnode<T>(car: NodeId, cdr: NodeId, token: T) -> Brzop<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    Brzop::Cat(Node::new(car, cdr, None, token))
}

pub fn repnode<T>(car: NodeId, token: T) -> Brzop<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    Brzop::Repeat(Node::new(car, 0, None, token))
}

impl<T> Barre<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
{
    pub fn new() -> Barre<T> {
        // Currently, this recognizer recognizes no strings of tokens
        let new_representation = vec![Brzop::Epsilon, Brzop::Empty];
        Barre::<T> {
            // original: new_representation.clone(),
            language: new_representation,
            memo: HashMap::new(),
            start: 1,
            epsilon: 0,
            empty: 1,
        }
    }

    pub fn from_language(lang: &Language<T>) -> Barre<T>
    {

        fn language_to_barre<T>(lang: &Language<T>) -> Vec<Brzop<T>>
            where
            T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
        {
            let mut new_representation = vec![Brzop::Epsilon, Brzop::Empty];

            fn language_handler<T>(lang: &Language<T>, r: &mut Vec<Brzop<T>>) -> NodeId
            where
                T: std::clone::Clone + std::cmp::PartialEq + std::cmp::Eq + std::fmt::Debug + std::fmt::Display + std::default::Default + std::hash::Hash,
            {
                match lang {
                    Language::Epsilon => 0,
                    Language::Token(ref t) => {
                        r.push(toknode(t.0.clone()));
                        r.len() - 1
                    }
                    Language::Alt(ref node) => {
                        let car = language_handler(&node.0, r);
                        let cdr = language_handler(&node.1, r);
                        r.push(altnode(car, cdr, T::default()));
                        r.len() - 1
                    }
                    Language::Cat(ref node) => {
                        let car = language_handler(&node.0, r);
                        let cdr = language_handler(&node.1, r);
                        r.push(catnode(car, cdr, T::default()));
                        r.len() - 1
                    }

                    Language::Repeat(ref node) => {
                        let car = language_handler(&node.0, r);
                        r.push(repnode(car, T::default()));
                        r.len() - 1
                    }
                }
            }

            language_handler(&lang, &mut new_representation);
            new_representation
        }

        let new_representation = language_to_barre(lang);
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

    fn push(&mut self, node: Brzop<T>) -> NodeId {
        self.language.push(node);
        self.len()
    }

    fn push_alt_derivative(&mut self, node: &Node<T>, token: T) -> NodeId {
        let car = self.derive(node.car, &token);
        let cdr = self.derive(node.cdr, &token);
        self.push(altnode(car, cdr, token))
    }

    // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
    // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
    fn push_cat_derivative(&mut self, node: &Node<T>, token: T) -> NodeId {
        let car = self.derive(node.car, &token);
        let lhs = self.push(catnode(car, node.cdr, token.clone()));
        if self.nullable(node.car) {
            let rhs = self.derive(node.cdr, &token);
            self.push(altnode(lhs, rhs, token.clone()))
        } else {
            lhs
        }
    }

    fn push_rep_derivative(&mut self, node: &Node<T>, token: T, parent: NodeId) -> NodeId {
        let car = self.derive(node.car, &token);
        self.push(catnode(car, parent, T::default()))
    }

    fn derive(&mut self, nodeid: NodeId, token: &T) -> NodeId {
        // If we have already seen this node, go get it and process it.

        if let Some(cached_node) = self.memo.get(&(nodeid, token.clone())) {
            return *cached_node;
        };

        let respective_derivative = match self.language[nodeid].clone() {
            // Dc(∅) = ∅
            Brzop::Empty => self.empty,

            // Dc(ε) = ∅
            Brzop::Epsilon => self.empty,

            // Dc(c) = ε if c = c'
            // Dc(c') = ∅ if c ≠ c'
            Brzop::Token(ref node) => {
                if node.token == *token {
                    self.epsilon
                } else {
                    self.empty
                }
            }

            // Dc(re1 | re2) = Dc(re1) | Dc(re2)
            Brzop::Alt(ref node) => {
                self.push_alt_derivative(&node, token.clone())
            }

            // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
            // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
            Brzop::Cat(ref node) => {
                self.push_cat_derivative(&node, token.clone())
            }

            // Dc(re*) = Dc(re) re*
            Brzop::Repeat(node) => {
                self.push_rep_derivative(&node, token.clone(), nodeid)
            }
        };
        self.memo.insert((nodeid, token.clone()), respective_derivative);
        respective_derivative
    }

    fn nullable(&self, node: NodeId) -> bool {
        match self.language[node] {
            Brzop::Empty => false,
            Brzop::Epsilon => true,
            Brzop::Token(_) => false,
            Brzop::Alt(ref node) => self.nullable(node.car) || self.nullable(node.cdr),
            Brzop::Cat(ref node) => self.nullable(node.car) && self.nullable(node.cdr),
            Brzop::Repeat(_) => true,
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
                    let nl = self.language[np].clone();
                    match nl {
                        Brzop::Empty => break false,
                        Brzop::Epsilon => match items.peek() {
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
    use builder::{cat, alt, rep, tok};

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
