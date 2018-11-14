use std::fmt;

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

#[derive(Clone, Debug)]
pub enum Nullable {
    Accept,
    Reject,
    InProgress,
    Unvisited,
}

#[derive(Clone, Debug)]
pub struct Emp {
    pub nullable: Nullable,
}

impl Emp {
    pub fn new() -> Emp {
        Emp {
            nullable: Nullable::Reject,
        }
    }
}

pub trait TokenClass<T> {
    fn has(&self, t: &T) -> bool;
}

#[derive(Clone, Debug)]
pub struct TokenPredicate<T>
    where T: Siaa
{
    content: T
}

impl<T> TokenClass<T> for TokenPredicate<T>
    where T: Siaa
{
    fn has(&self, t: &T) -> bool {
        self.content == *t
    }
}

#[derive(Clone, Debug)]
pub struct Eps<T: Siaa> {
    pub content: T,
    pub nullable: Nullable,
}

impl<T: Siaa> Eps<T> {
    pub fn new(t: T) -> Eps<T> {
        Eps {
            content: t,
            nullable: Nullable::Accept,
        }
    }
}

#[derive(Clone)]
pub struct Tok<T: Siaa> {
    pub content: T,
    pub predicate: TokenPredicate<T>,
    pub nullable: Nullable,
}

impl<T: Siaa + 'static> Tok<T> {
    pub fn new(t: T) -> Tok<T> {
        Tok {
            content: t.clone(),
            predicate: TokenPredicate { content: t },
            nullable: Nullable::Reject,
        }
    }

    pub fn class(p: TokenPredicate<T>) -> Tok<T> {
        Tok {
            content: T::default(),
            predicate: p,
            nullable: Nullable::Reject,
        }
    }
}

impl<T: Siaa> fmt::Debug for Tok<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Tok {{ content: {:?}, nullable: {:?} }}",
            self.content, self.nullable
        )
    }
}

#[derive(Clone, Debug)]
pub struct Alt {
    pub left: NodeId,
    pub right: NodeId,
    pub nullable: Nullable,
}

impl Alt {
    pub fn new(left: NodeId, right: NodeId) -> Alt {
        Alt {
            left,
            right,
            nullable: Nullable::Unvisited,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Cat {
    pub left: NodeId,
    pub right: NodeId,
    pub nullable: Nullable,
}

impl Cat {
    pub fn new(left: NodeId, right: NodeId) -> Cat {
        Cat {
            left,
            right,
            nullable: Nullable::Unvisited,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Rep {
    pub child: NodeId,
    pub nullable: Nullable,
}

impl Rep {
    pub fn new(child: NodeId) -> Rep {
        Rep {
            child,
            nullable: Nullable::Accept,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Laz<T: Siaa> {
    pub parent: NodeId,
    pub content: T,
    pub nullable: Nullable,
}

impl<T: Siaa> Laz<T> {
    pub fn new(parent: NodeId, content: T) -> Laz<T> {
        Laz {
            parent,
            content,
            nullable: Nullable::Unvisited,
        }
    }
}

/// The basic node.
#[derive(Clone, Debug)]
pub enum Node<T: Siaa> {
    Emp(Emp),
    Eps(Eps<T>),
    Tok(Tok<T>),
    Alt(Alt),
    Cat(Cat),
    Rep(Rep),
    Laz(Laz<T>),
}

impl<T: Siaa> Node<T> {
    pub fn is_nullable(&self) -> Nullable {
        use self::Node::*;
        match self {
            Emp(ref a) => a.nullable.clone(),
            Eps(ref a) => a.nullable.clone(),
            Tok(ref a) => a.nullable.clone(),
            Alt(ref a) => a.nullable.clone(),
            Cat(ref a) => a.nullable.clone(),
            Rep(ref a) => a.nullable.clone(),
            Laz(ref a) => a.nullable.clone(),
        }
    }

    pub fn set_nullable(&mut self, nullable: Nullable) {
        use self::Node::*;
        match self {
            Emp(ref mut a) => {
                a.nullable = nullable;
            }
            Eps(ref mut a) => {
                a.nullable = nullable;
            }
            Tok(ref mut a) => {
                a.nullable = nullable;
            }
            Alt(ref mut a) => {
                a.nullable = nullable;
            }
            Cat(ref mut a) => {
                a.nullable = nullable;
            }
            Rep(ref mut a) => {
                a.nullable = nullable;
            }
            Laz(ref mut a) => {
                a.nullable = nullable;
            }
        }
    }
}
