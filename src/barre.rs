use std::collections::HashMap;

use arena::{Arena, NodeId};

use language::Language;
use grammar::Grammar;
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
    ( $par:ident, $lhs:expr, $rhs:expr ) => {
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

    pub fn match_re<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut grammar = Grammar {
            arena: self.arena.clone(),
            memo: HashMap::new(),
            empty: self.empty,
            epsilon: self.epsilon,
        };

        grammar.parse(items, self.start)
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
