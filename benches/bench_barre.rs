#[macro_use]
extern crate criterion;

#[macro_use]
extern crate barre;

extern crate consy;
extern crate hashbrown;

use criterion::Criterion;
use self::consy::Cell;
use barre::Barre;
use barre::ParseTree;
use hashbrown::HashSet;

use barre::language::{alt, cat, tok};

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

fn extract_match(res: &Option<HashSet<ParseTree>>) -> Option<String> {
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

fn bench_more_complex_expression(c: &mut Criterion) {
    // /AB(CC|DDDD)E*F/

    c.bench_function("Complex Expressions", |c| {
        c.iter(|| {
            let lang = cat!(
                tok('a'),
                tok('b'),
                alt!(cat!(tok('c'), tok('c')), cat!(tok('d'), tok('d'), tok('d'), tok('d'))),
                tok('e'),
                tok('f')
            );
            let mut barre = Barre::from_language(&lang);

            testpat!(barre; [
            ("abccef", Some("abccef")),
            ("abccef", Some("abccef")),
            ("abddddef", Some("abddddef")),
            ("abddddef", Some("abddddef")),
            ("ab", None),
            ("abcef", None),
            ("abcdef", None),
            ("abcceff", None),
            ("", None),
            ("abcddf", None)
        ]);
        })
    });
}

criterion_group!(benches, bench_more_complex_expression);
criterion_main!(benches);
