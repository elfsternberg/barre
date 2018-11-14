#![warn(clippy::shadow_reuse, clippy::shadow_same, clippy::shadow_unrelated)]
#![allow(clippy::new_without_default_derive, clippy::redundant_field_names)]
use std::collections::HashMap;

use types::{Siaa, Node, NodeId};
use builder::{init_barre_vec, language_to_vec_rep};
use language::Language;
use grammar::Grammar;

pub struct Barre<T: Siaa> {
    language: Vec<Node<T>>,
    start: NodeId,
    empty: NodeId,
}

impl<T: Siaa + 'static> Barre<T> {
    pub fn new() -> Barre<T> {
        // Currently, this recognizer recognizes no strings of tokens
        let new_representation = init_barre_vec();
        let start = new_representation.len() - 1;

        Barre::<T> {
            language: new_representation,
            start: start,
            empty: start,
        }
    }

    pub fn from_language(lang: &Language<T>) -> Barre<T> {
        let new_representation = language_to_vec_rep(lang);
        let start = new_representation.len() - 1;

        Barre::<T> {
            language: new_representation,
            start: start,
            empty: 0,
        }
    }

    pub fn parse<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>
    {
        let mut grammar = Grammar {
            language: self.language.clone(),
            memo: HashMap::new(),
            listeners: HashMap::new(),
            start: self.start,
            empty: self.empty
        };
        
        grammar.parse(items)
    }
}

#[cfg(test)]
mod tests {

    use super::Barre;
    use language::{alt, cat, rep, tok};

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
                    assert!($pt.parse(&mut pair.0.chars()) == pair.1);
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
    fn simple_rep() {
        let lang = rep(tok('a'));
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [("b", false), ("a", true), ("aaaaaaa", true), ("", true)]);
    }

    #[test]
    fn slightly_more_complex_untyped_macro() {
        // /AB(CC|DDDD)E*F/
        let lang = cat!(
            tok('a'),
            tok('b'),
            alt!(
                cat!(tok('c'), tok('c')),
                cat!(tok('d'), tok('d'), tok('d'), tok('d'))
            ),
            rep(tok('e')),
            tok('f')
        );
        let mut barre = Barre::from_language(&lang);
        testpat!(barre; [
            ("abccf", true), ("abccef", true), ("abcceeeeeeef", true), ("abddddf", true),
            ("abddddef", true), ("abddddeeeeeef", true), ("ab", false), ("abcef", false),
            ("abcdef", false), ("abcceff", false), ("", false), ("abcddf", false)

        ]);
    }
}
