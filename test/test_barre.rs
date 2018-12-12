extern crate barre;
extern crate consy;
use barre::Barre;
use self::consy::Cell;
use std::collections::HashSet;
// use language::{cat, alt, rep, tok};

use barre::language::tok;

fn extract_match_inner(s: &mut String, pt: &Cell<char>) {
    match pt {
        Cell::Nil => {}
        Cell::Lit(t) => s.push(t.clone()),
        Cell::Pair(a, b) => {
            extract_match_inner(s, a);
            extract_match_inner(s, b);
        }
    }
}

fn extract_match(res: &Option<HashSet<Cell<char>>>) -> Option<String> {
    let mut ret = String::new();
    println!("{:?}", res);
    if let Some(r) = res {
        let mut it = r.iter();
        if let Some(pt) = it.next() {
            extract_match_inner(&mut ret, &pt);
            println!("{:?}", ret);
            return Some(ret);
        }
    }
    None
}

macro_rules! mkpair {
    ($(($l:expr, $r:expr)),*) => {
        vec![$(($l, $r)),*]
            .into_iter()
            .map(|pair| { (String::from(pair.0), match pair.1 { None => None, Some(a) => Some(String::from(a)) })});
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

#[test]
fn just_a_token() {
    let lang = tok('a');
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("a", Some("a")),
                     ("b", None),
                     ("", None),
                     ("ab", None)]);
}

#[test]
fn just_an_alt() {
    let lang = alt!(tok('a'), tok('b'), tok('c'), tok('d'));
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("a", Some("a")),
                     ("b", Some("b")),
                     ("d", Some("d")),
                     ("", None),
                     ("ab", None),
                     ("ba", None)]);
}

#[test]
fn just_a_cat() {
    let lang = cat!(tok('a'), tok('b'), tok('c'));
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("abc", Some("abc")),
                     ("", None), ("b", None),
                     ("aab", None), ("aba", None), ("a", None)
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
        ("foo", Some("foo")), ("bar", Some("bar")), ("baz", Some("baz")),
        ("far", None), ("boo", None), ("ba", None),
        ("foobar", None), ("", None)
    ]);
}

#[test]
fn slightly_more_complex_untyped_macro() {
    // /AB(CC|DDDD)E*F/
    let lang = cat!(
        tok('a'),
        tok('b'),
        alt!(cat!(tok('c'), tok('c')), cat!(tok('d'), tok('d'), tok('d'), tok('d'))),
        tok('e'),
        tok('f')
    );
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("abddddef", Some("abddddef"))]);
//     testpat!(barre; [
//         ("abccef", Some("abccef")),
//         ("abccef", Some("abccef")),
//         ("abddddef", Some("abddddef")),
//         ("abddddef", Some("abddddef")),
//         ("ab", None),
//         ("abcef", None),
//         ("abcdef", None),
//         ("abcceff", None),
//         ("", None),
//         ("abcddf", None)
//     ]);
}

#[test]
fn repro() {
    let lang = cat!(alt!(cat!(tok('b'), tok('c')), cat!(tok('d'), tok('e'))), tok('f'));
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("bcf", Some("bcf"))]);
}    

#[test]
fn graphit() {
    let lang = alt!(
        cat!(tok('f'), tok('o'), tok('o')),
        cat!(tok('b'), tok('a'), tok('r')),
        cat!(tok('b'), tok('a'), tok('z'))
    );
    let mut barre = Barre::from_language(&lang);
    testpat!(barre; [("bar", Some("bar"))]);
}
