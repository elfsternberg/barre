extern crate hashbrown;
use arena::{Arena, NodeId};
use barre::builder::{init_barre_arena};
use barre::grammar::{Grammar, parser_default_nullable, Nullable};
use barre::types::Parser;
use hashbrown::{HashMap};
use consy::Cell;

pub fn init_nulls(arena: &Arena<Parser>) -> Vec<Nullable> {
    arena.iter().map(|t| parser_default_nullable(&t.data)).collect()
}

pub fn init_grammar() -> Grammar {
    let arena = init_barre_arena();
    let nulls = init_nulls(&arena);
    
    Grammar {
        arena: arena,
        nulls: nulls,
        store: vec![],
        memo: HashMap::new(),
        listeners: HashMap::new(),
        empty: 1,
    }
}
    
#[test]
fn simple_cat() {
    let mut grammar = init_grammar();
    let a = grammar.add(Parser::Tok('a'));
    let b = grammar.add(Parser::Tok('b'));
    let c = grammar.make_optimized_cat(a, b);
    println!("{:?}", grammar.arena);
    assert!(true);
}

#[test]
fn left_optimized_cat_for_cat() {
    let mut grammar = init_grammar();
    let (a, b) = (grammar.add(Parser::Tok('a')), grammar.add(Parser::Tok('b')));
    let c = grammar.make_optimized_cat(a, b);
    let d = grammar.add(Parser::Tok('c'));
    let e = grammar.make_optimized_cat(c, d);
    println!("{:?}", grammar.arena);
    let p = grammar.parse(&mut "abc".chars(), e).unwrap().0.into_iter().next().unwrap();
    // Expect the leftmost item to be a pair, as c is a pair, above.
    assert!(p.car().unwrap().pairp());
}

#[test]
fn left_optimized_cat_for_eps() {
    let mut grammar = init_grammar();
    let (a, b) = (grammar.make_eps(&'a'), grammar.add(Parser::Tok('b')));
    let c = grammar.make_optimized_cat(a, b);
    println!("{:?}", grammar.arena);
    let p = grammar.parse(&mut "b".chars(), c).unwrap().0.into_iter().next().unwrap();
    assert!(p.car().unwrap() == &Cell::Lit('a'));
    assert!(p.cdr().unwrap() == &Cell::Lit('b'));
}


