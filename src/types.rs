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

#[derive(Clone, Copy, Debug)]
pub enum Parser<T: Siaa> {
    Unk,
    Emp,
    Eps(T),
    Del,
    Tok(T),
    Alt,
    Cat,
    //    Rep,
    Laz(T),
}
