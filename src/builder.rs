use language::Language;
use types::{Alt, Cat, Emp, Eps, Node, NodeId, Rep, Siaa, Tok};

pub fn init_barre_vec<T: Siaa>() -> Vec<Node<T>> {
    vec![Node::Emp(Emp::new())]
}

pub fn language_to_vec_rep<T: Siaa + 'static>(lang: &Language<T>) -> Vec<Node<T>> {
    let mut new_representation = init_barre_vec();

    fn language_handler<T: Siaa + 'static>(lang: &Language<T>, r: &mut Vec<Node<T>>) -> NodeId {
        match lang {
            Language::Epsilon => {
                r.push(Node::Eps(Eps::new(T::default())));
                r.len() - 1
            }

            Language::Token(ref t) => {
                r.push(Node::Tok(Tok::new(t.0.clone())));
                r.len() - 1
            }

            Language::Alt(ref node) => {
                let car = language_handler(&node.0, r);
                let cdr = language_handler(&node.1, r);
                r.push(Node::Alt(Alt::new(car, cdr)));
                r.len() - 1
            }

            Language::Cat(ref node) => {
                let car = language_handler(&node.0, r);
                let cdr = language_handler(&node.1, r);
                r.push(Node::Cat(Cat::new(car, cdr)));
                r.len() - 1
            }

            Language::Repeat(ref node) => {
                let car = language_handler(&node.0, r);
                r.push(Node::Rep(Rep::new(car)));
                r.len() - 1
            }
        }
    }

    language_handler(&lang, &mut new_representation);
    new_representation
}
