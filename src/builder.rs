use language::Language;
use types::{Node, NodeId, Siaa};

pub fn init_barre_vec<T: Siaa>() -> Vec<Node<T>> {
    vec![Node::Eps(T::default()), Node::Emp]
}

pub fn language_to_vec_rep<T: Siaa>(lang: &Language<T>) -> Vec<Node<T>> {
    let mut new_representation = init_barre_vec();

    fn language_handler<T: Siaa>(lang: &Language<T>, r: &mut Vec<Node<T>>) -> NodeId {
        match lang {
            Language::Epsilon => 0,

            Language::Token(ref t) => {
                r.push(Node::Tok(t.0.clone()));
                r.len() - 1
            }

            Language::Alt(ref node) => {
                let car = language_handler(&node.0, r);
                let cdr = language_handler(&node.1, r);
                r.push(Node::Alt(car, cdr));
                r.len() - 1
            }

            Language::Cat(ref node) => {
                let car = language_handler(&node.0, r);
                let cdr = language_handler(&node.1, r);
                r.push(Node::Cat(car, cdr));
                r.len() - 1
            }

            Language::Repeat(ref node) => {
                let car = language_handler(&node.0, r);
                r.push(Node::Rep(car));
                r.len() - 1
            }
        }
    }

    language_handler(&lang, &mut new_representation);
    new_representation
}
