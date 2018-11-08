pub type NodeId = usize;

/// Symbol in an alphabet.
///
/// This marker trait for all the qualities that a token must support in
/// order to be useable in a regular expression.  All of these traits
/// are supported by "char" and "u8", unsurprisingly.
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

/// The basic node.
#[derive(Clone, Copy, Debug)]
pub enum Node<T: Siaa> {
    Emp,
    Eps(T),
    Tok(T),
    Alt(NodeId, NodeId),
    Cat(NodeId, NodeId),
    Rep(NodeId),
    Laz(NodeId, T),
}
