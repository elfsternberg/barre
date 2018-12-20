use parsesets::{ParseSet, RedFn};
use std::fmt;
use std::rc::Rc;

/// Symbol in an alphabet.
///
/// This marker trait for all the qualities that a token must support in
/// order to be useable in a regular expression.  All of these traits
/// are supported by "char" and "u8", unsurprisingly.
/*
pub trait Siaa:
    std::clone::Clone
    + std::cmp::PartialEq
    + std::cmp::Eq
    + std::fmt::Debug
    + std::fmt::Display
    + std::default::Default
    + std::hash::Hash
{
}

impl<T> Siaa for T where
    T: std::clone::Clone
        + std::cmp::PartialEq
        + std::cmp::Eq
        + std::fmt::Debug
        + std::fmt::Display
        + std::default::Default
        + std::hash::Hash
{}
*/

#[derive(Clone)]
pub enum Parser {
    Emp,
    Eps(Rc<ParseSet>),
    Tok(char),
    Alt,
    Cat,
    Red(Rc<RedFn>),
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

impl fmt::Debug for Parser {
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
