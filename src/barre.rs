use std::collections::HashMap;

use arena::{Arena, NodeId};

use language::Language;
use builder::{init_barre_arena, language_to_arena};
use types::{Siaa, Parser};

pub struct Barre<T:Siaa>
{
    arena: Arena<Parser<T>>,
    memo: HashMap<(NodeId, T), NodeId>,
    start: NodeId,
    empty: NodeId,
    epsilon: NodeId,
}

macro_rules! add_node {
    ( $par:ident, $lhs:expr, $rhs: expr ) => {
        {
            let newparser = self.arena.add($par);
            self.arena[newparser].left = $lhs;
            self.arena[newparser].right = $rhs;
            newparser
        }
    };
}

impl<T: Siaa> Barre<T>
{
    pub fn from_arena(arena: Arena<Parser<T>>, start: NodeId) -> Barre<T> {
        Barre::<T> {
            arena: arena,
            memo: HashMap::new(),
            start: start,
            epsilon: 2,
            empty: 1,
        }
    }
        
    pub fn from_language(lang: &Language<T>) -> Barre<T>
    {
        let arena_start = language_to_arena(lang);
        Barre::from_arena(arena_start.0, arena_start.1)
    }

    pub fn new() -> Barre<T> {
        Barre::from_arena(init_barre_arena(), 1)
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

    pub fn match_re<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut items = items.peekable();
        let mut current_node = self.start;
        println!("START: {:?}", self.start);
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
        let lang = cat!(tok('a'), tok('b'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("ab", true), ("", false), ("b", false),
            ("aab", false), ("aba", false), ("a", false)
        ]);
    }

    #[test]
    fn just_an_alt() {
        let lang = alt!(tok('a'), tok('b'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [("a", true), ("b", true), ("ab", false), ("", false)]);
    }

    #[test]
    fn just_a_rep() {
        let lang = rep(tok('a'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [("a", true), ("", true), ("aaaaaa", true), ("aaaaab", false)]);
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
