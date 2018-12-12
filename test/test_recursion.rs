use arena::{Arena, NodeId};
use barre::builder::init_barre_arena;
use barre::grammar::{parser_default_nullable, Grammar, Nullable};
use barre::types::Parser;
use consy::Cell;
use hashbrown::HashMap;

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
fn beer() {
    let mut grammar = init_grammar();
    let b = grammar.add(Parser::Tok('b'));
    let ee = {
        let e1 = grammar.add(Parser::Tok('e'));
        let e2 = grammar.add(Parser::Tok('e'));
        grammar.make_optimized_cat(e1, e2)
    };
    let eestar = grammar.make_kleene_star(ee);
    let r = grammar.add(Parser::Tok('r'));
    let eer = grammar.make_optimized_cat(eestar, r);
    let beer = grammar.make_optimized_cat(b, eer);

    let p = grammar
        .parse(&mut "beeeer".chars(), beer)
        .unwrap()
        .0
        .into_iter()
        .next()
        .unwrap();
    println!("{:?}", p);
    assert!(true);
}
