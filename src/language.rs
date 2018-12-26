use std::fmt;

#[derive(PartialEq, Debug)]
pub struct Token<T: ISiaa>(pub T);

#[derive(PartialEq, Debug)]
pub struct Alt<T: ISiaa>(pub Box<Language<T>>, pub Box<Language<T>>);

#[derive(PartialEq, Debug)]
pub struct Cat<T: ISiaa>(pub Box<Language<T>>, pub Box<Language<T>>);

// #[derive(PartialEq, Debug)]
// pub struct Repeat(pub Box<Language>)
// where
//     T: std::fmt::Display + std::fmt::Debug;

pub trait ISiaa: std::fmt::Debug {}
impl<T> ISiaa for T where T: std::fmt::Debug {}

#[derive(PartialEq, Debug)]
pub enum Language<T: ISiaa> {
    Epsilon,
    Token(Token<T>),
    Alt(Alt<T>),
    Cat(Cat<T>),
    //     Repeat(Repeat),
}

pub fn icat<T: ISiaa>(l: Language<T>, r: Language<T>) -> Language<T> {
    Language::Cat(Cat(Box::new(l), Box::new(r)))
}

pub fn ialt<T: ISiaa>(l: Language<T>, r: Language<T>) -> Language<T> {
    Language::Alt(Alt(Box::new(l), Box::new(r)))
}

// pub fn rep(n: Language) -> Language
// where
//     T: std::fmt::Display + std::fmt::Debug,
// {
//     Language::Repeat(Repeat(Box::new(n)))
// }
//

pub fn tok<T: ISiaa>(t: T) -> Language<T> {
    Language::Token(Token(t))
}

pub fn eps<T: ISiaa>() -> Language<T> {
    Language::Epsilon
}

#[macro_export]
macro_rules! cat {
    ($l:expr, $r:expr) => {
        $crate::language::icat($l, $r)
    };

    ($l:expr, $r:expr, $($x:expr),*) => {
        $crate::language::icat($l, cat!($r, $($x),*))
    };
}

#[macro_export]
macro_rules! alt {
    ($l:expr, $r:expr) => {
        $crate::language::ialt($l, $r)
    };

    ($l:expr, $r:expr, $($x:expr),*) => {
        $crate::language::ialt($l, alt!($r, $($x),*))
    };

}

impl<T: ISiaa> fmt::Display for Alt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn alt_helper<T: ISiaa>(f: &mut fmt::Formatter, alt: &Alt<T>) -> fmt::Result {
            write!(f, "(")?;
            alt.0.fmt(f)?;
            write!(f, "|")?;

            let mut c = &*alt.1;
            loop {
                let p = match c {
                    Language::Alt(nalt) => {
                        nalt.0.fmt(f)?;
                        write!(f, "|")?;
                        &nalt.1
                    }
                    _ => {
                        c.fmt(f)?;
                        write!(f, ")")?;
                        break;
                    }
                };
                c = p;
            }
            write!(f, "")
        }
        alt_helper(f, self)
    }
}

impl<T: ISiaa> fmt::Display for Cat<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        write!(f, "{}", self.1)
    }
}

impl<T: ISiaa> fmt::Display for Language<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Language::Epsilon => write!(f, "ε"),
            Language::Token(ref c) => write!(f, "{:?}", c.0),
            Language::Alt(ref alt) => write!(f, "{}", alt),
            Language::Cat(ref cat) => write!(f, "{}", cat),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_patterns() {
        let pattern = tok('A');
        assert_eq!(pattern, Language::Token(Token('A')));
    }
}
