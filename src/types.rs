use parsesets::{Forest, IRedFn};
use siaa::{Siaa, Riaa};
use std::fmt;
use std::rc::Rc;

// A Parser is an object with just three functions:
// * `derive`, which renders a derivative of the the parser,
// * `nullable`, which renders whether or not the parser is nullable,
// * `parse_tree`, which returns the structure of a successful parse, if any.

// See the Grammar seccion

#[derive(Clone)]
pub enum Parser<T: Siaa, U: Riaa<T>> {
    Emp,
    Eps(Rc<Forest<U>>),
    Tok(T),
    Alt,
    Cat,
    Red(Rc<IRedFn<T, U>>),
    Ukn,
    //Rep, // Repetition
    //Its  // Intersection - This *and* that
    //Neg  // Negation - *Not* this
    //Lea  // Interleaf - A variant of the concatenation operator in
    //        which the contents of the concatenation may appear in
    //        any order.  This can support declarative syntaxes
    //        such as the properties of an XML/HTML tag.
    //
    //Cut  // The Cut Operator - A variant of the concatenation operator
    //        in which, if the left operation has an indeterminate
    //        number of tokens, always matches the maximal number
    //        of tokens possible.  I wonder if this is a way to
    //        meet Sulzmann's POSIX requirement?
}

impl<T: Siaa, U: Riaa<T>> fmt::Debug for Parser<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Parser::Emp => write!(f, "Emp"),
            Parser::Eps(n) => write!(f, "Eps: {:?}", n),
            Parser::Tok(c) => write!(f, "Tok: {:?}", c),
            Parser::Alt => write!(f, "Alt"),
            Parser::Cat => write!(f, "Cat"),
            Parser::Red(_) => write!(f, "Red"),
            Parser::Ukn => write!(f, "Ukn"),
        }
    }
}
