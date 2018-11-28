use arena::{Arena, NodeId};
use language::Language;
use types::{Parser, Siaa};

pub fn init_barre_arena<T: Siaa>() -> Arena<Parser<T>> {
    let mut arena = Arena::new();
    let _ = arena.add(Parser::Emp);
    let _ = arena.add(Parser::Emp);
    arena
}

pub fn language_to_arena<T: Siaa>(lang: &Language<T>) -> (Arena<Parser<T>>, NodeId) {
    let mut new_representation = init_barre_arena();

    fn language_handler<T: Siaa>(lang: &Language<T>, r: &mut Arena<Parser<T>>) -> NodeId {
        match lang {
            Language::Epsilon => r.add(Parser::Eps(T::default())),

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
            },

            //             Language::Repeat(ref node) => {
            //                 let ent = r.add(Parser::Rep);
            //                 r[ent].left = language_handler(&node.0, r);
            //                 ent
            //             }
        }
    }

    let start = language_handler(&lang, &mut new_representation);
    (new_representation, start)
}
