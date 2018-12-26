/// Symbol in an alphabet.
///
/// This is a marker trait for all the qualities that a token must
/// support in order to be useable in a regular expression.  All of
/// these traits are supported by "char" and "u8", unsurprisingly.
///
/// This mirrors the Haskell code that says that tokens must be
/// (Eq, Show, Ord). For our purposes, it must also be hashable.

pub trait Siaa:
    std::clone::Clone + std::cmp::Eq + std::default::Default + std::fmt::Debug + std::hash::Hash
{
}

impl<T> Siaa for T where
    T: std::clone::Clone + std::cmp::Eq + std::default::Default + std::fmt::Debug + std::hash::Hash
{
}

pub trait Riaa<U>:
    std::clone::Clone + std::cmp::Eq + std::default::Default + std::fmt::Debug + std::hash::Hash
{
}

impl<T, U> Riaa<U> for T
where
    T: std::clone::Clone + std::cmp::Eq + std::default::Default + std::fmt::Debug + std::hash::Hash,
    U: std::convert::From<T>,
{
}
