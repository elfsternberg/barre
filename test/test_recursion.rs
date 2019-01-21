use barre::Barre;
use barre::barre::cast;
use barre::Grammar;


#[test]
fn beer() {
    let mut grammar = Grammar::<char, char>::new(cast);
    let b = grammar.make_tok(&'b');
    let ee = {
        let e1 = grammar.make_tok(&'e');
        let e2 = grammar.make_tok(&'e');
        grammar.make_cat(e1, e2)
    };
    let eestar = grammar.make_rep(ee);
    let r = grammar.make_tok(&'r');
    let eer = grammar.make_cat(eestar, r);
    let beer = grammar.make_cat(b, eer);
    grammar.start = beer;
    let mut barre = Barre::from_grammar(grammar);
    let p = barre.parse(&mut "beeeer".chars()).unwrap().into_iter().next().unwrap();
    println!("{:?}", p);
    assert!(true);
}
