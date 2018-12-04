use arena::{Arena, NodeId};
use language::Language;
use types::Parser;

pub fn init_barre_arena() -> Arena<Parser> {
    let mut arena = Arena::new();
    let _ = arena.add(Parser::Emp);
    let _ = arena.add(Parser::Emp);
    let _ = arena.add(Parser::Eps(0));
    arena
}

pub fn language_to_arena(lang: &Language) -> (Arena<Parser>, NodeId) {
    let mut new_representation = init_barre_arena();

    fn language_handler(lang: &Language, r: &mut Arena<Parser>) -> NodeId {
        match lang {
            Language::Epsilon => r.add(Parser::Eps(2)),

            Language::Token(ref t) => r.add(Parser::Tok(t.0.clone())),

            Language::Alt(ref node) => {
                let ent = r.add(Parser::Alt);
                r[ent].left = language_handler(&node.0, r);
                r[ent].right = language_handler(&node.1, r);
                ent
            }
            Language::Cat(ref node) => {
                let ent = r.add(Parser::Cat);
                r[ent].left = language_handler(&node.0, r);
                r[ent].right = language_handler(&node.1, r);
                ent
            }
        }
    }

    let start = language_handler(&lang, &mut new_representation);
    (new_representation, start)
}
