use std::fmt;

#[derive(PartialEq, Debug)]
pub struct Token(pub char);

#[derive(PartialEq, Debug)]
pub struct Alt(pub Box<Language>, pub Box<Language>);

#[derive(PartialEq, Debug)]
pub struct Cat(pub Box<Language>, pub Box<Language>);

// #[derive(PartialEq, Debug)]
// pub struct Repeat(pub Box<Language>)
// where
//     T: std::fmt::Display + std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub enum Language {
    Epsilon,
    Token(Token),
    Alt(Alt),
    Cat(Cat),
    //     Repeat(Repeat),
}

pub fn cat(l: Language, r: Language) -> Language {
    Language::Cat(Cat(Box::new(l), Box::new(r)))
}

pub fn alt(l: Language, r: Language) -> Language {
    Language::Alt(Alt(Box::new(l), Box::new(r)))
}

// pub fn rep(n: Language) -> Language
// where
//     T: std::fmt::Display + std::fmt::Debug,
// {
//     Language::Repeat(Repeat(Box::new(n)))
// }
//

pub fn tok(t: char) -> Language {
    Language::Token(Token(t))
}

pub fn eps() -> Language {
    Language::Epsilon
}

#[macro_export]
macro_rules! cat {
    ($l:expr, $r:expr) => {
        $crate::language::cat($l, $r)
    };

    ($l:expr, $r:expr, $($x:expr),*) => {
        $crate::language::cat($l, cat!($r, $($x),*))
    };
}

#[macro_export]
macro_rules! alt {
    ($l:expr, $r:expr) => {
        $crate::language::alt($l, $r)
    };

    ($l:expr, $r:expr, $($x:expr),*) => {
        $crate::language::alt($l, alt!($r, $($x),*))
    };

}

impl fmt::Display for Alt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn alt_helper(f: &mut fmt::Formatter, alt: &Alt) -> fmt::Result {
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

impl fmt::Display for Cat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        write!(f, "{}", self.1)
    }
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Language::Epsilon => write!(f, "ε"),
            Language::Token(ref c) => write!(f, "{}", c.0),
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
