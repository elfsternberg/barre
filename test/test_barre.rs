extern crate barre;
use barre::types::Siaa;
use barre::{Barre, ParseTree};
use std::collections::HashSet;
// use language::{cat, alt, rep, tok};

use barre::language::tok;

fn extract_match_inner(s: &mut String, pt: &ParseTree<char>) {
    match pt {
        ParseTree::Nil => {}
        ParseTree::Lit(t) => s.push(t.clone()),
        ParseTree::Pair(a, b) => {
            extract_match_inner(s, a);
            extract_match_inner(s, b);
        }
    }
}

fn extract_match(res: &Option<HashSet<ParseTree<char>>>) -> Option<String> {
    let mut ret = String::new();
    if let Some(r) = res {
        let mut it = r.iter();
        if let Some(pt) = it.next() {
            extract_match_inner(&mut ret, &pt);
            return Some(ret);
        }
    }
    None
}

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
                let res = $pt.parse(&mut pair.0.chars());
                assert!(extract_match(&res) == pair.1);
            }
        }
    }
}

//    #[test]
//    fn show_your_work() {
//        let lang = alt!(
//            cat!(tok('f'), tok('o'), tok('o')),
//            cat!(tok('b'), tok('a'), tok('r')),
//            cat!(tok('b'), tok('a'), tok('z'))
//        );
//        let mut barre = Barre::from_language(&lang);
//        println!("{:?}", barre.parse(&mut String::from("bar").chars()));
//        assert!(true);
//    }

#[test]
fn just_a_token() {
    let lang = tok('a');
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("a", Some("a".to_string())),
                     ("b", None),
                     ("", None),
                     ("ab", None)]);
}

#[test]
fn just_an_alt() {
    let lang = alt!(tok('a'), tok('b'), tok('c'), tok('d'));
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("a", Some("a".to_string())),
                     ("b", Some("b".to_string())),
                     ("d", Some("d".to_string())),
                     ("", None),
                     ("ab", None),
                     ("ba", None)]);
}

#[test]
fn just_a_cat() {
    let lang = cat!(tok('a'), tok('b'), tok('c'));
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("abc", Some("abc".to_string()))]);
    //a, ("", None), ("b", None),
    //("aab", None), ("aba", None), ("a", None)
    // ]);
}

//     #[test]
//     fn just_an_alt() {
//         let lang = alt!(tok('a'), tok('b'));
//         let mut barre = Barre::from_language(&lang);
//         testpat!(barre; [("a", true), ("b", true), ("ab", false), ("", false)]);
//     }

//     #[test]
//     fn just_a_rep() {
//         let lang = rep(tok('a'));
//         let mut barre = Barre::from_language(&lang);
//         testpat!(barre; [("a", true), ("", true), ("aaaaaa", true), ("aaaaab", false)]);
//     }

//     #[test]
//     fn mildly_complex_macro_pattern() {
//         let lang = alt!(
//             cat!(tok('f'), tok('o'), tok('o')),
//             cat!(tok('b'), tok('a'), tok('r')),
//             cat!(tok('b'), tok('a'), tok('z'))
//         );
//         let mut barre = Barre::from_language(&lang);
//         testpat!(barre; [
//             ("foo", true), ("bar", true), ("baz", true),
//             ("far", false), ("boo", false), ("ba", false),
//             ("foobar", false), ("", false)
//         ]);
//     }

//     #[test]
//     fn slightly_more_complex_untyped_macro() {
//         // /AB(CC|DDDD)E*F/
//         let lang = cat!(tok('a'), tok('b'),
//                         alt!(cat!(tok('c'), tok('c')),
//                              cat!(tok('d'), tok('d'), tok('d'), tok('d'))),
//                         rep(tok('e')), tok('f'));
//         let mut barre = Barre::from_language(&lang);
//         testpat!(barre; [
//             ("abccf", true), ("abccef", true), ("abcceeeeeeef", true), ("abddddf", true),
//             ("abddddef", true), ("abddddeeeeeef", true), ("ab", false), ("abcef", false),
//             ("abcdef", false), ("abcceff", false), ("", false), ("abcddf", false)

//         ]);
//     }
