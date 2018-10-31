use std::fmt;

#[derive(PartialEq, Debug)]
pub struct Token<T>(T)
    where T: std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub struct Alt<T>(Box<Language<T>>, Box<Language<T>>)
where
    T: std::fmt::Display + std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub struct Cat<T>(Box<Language<T>>, Box<Language<T>>)
where
    T: std::fmt::Display + std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub struct Repeat<T>(Box<Language<T>>)
where
    T: std::fmt::Display + std::fmt::Debug;

#[derive(PartialEq, Debug)]
pub enum Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Epsilon,
    LToken(Token<T>),
    Alt(Alt<T>),
    Cat(Cat<T>),
    Repeat(Repeat<T>),
}

pub fn cat<T>(l: Language<T>, r: Language<T>) -> Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Language::Cat(Cat(Box::new(l), Box::new(r)))
}

pub fn alt<T>(l: Language<T>, r: Language<T>) -> Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Language::Alt(Alt(Box::new(l), Box::new(r)))
}

pub fn rep<T>(n: Language<T>) -> Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Language::Repeat(Repeat(Box::new(n)))
}

pub fn tok<T>(t: T) -> Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Language::LToken(Token(t))
}

pub fn eps<T>() -> Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    Language::Epsilon
}

#[allow(unused_macros)]
macro_rules! cat {
    ($l:expr, $r:expr, $($x:expr),+) => {
        cat($l, cat!($r, $($x)*))
    };

    ($l:expr, $r:expr) => {
        cat($l, $r)
    };
}

#[allow(unused_macros)]
macro_rules! alt {
    ($l:expr, $r:expr, $($x:expr),+) => {
        alt($l, alt!($r, $($x)*))
    };

    ($l:expr, $r:expr) => {
        alt($l, $r)
    };
}

impl fmt::Display for Token<char>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> fmt::Display for Token<T>
    where T: std::fmt::Debug
{
    default fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}


impl<T> fmt::Display for Alt<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn alt_helper<T>(f: &mut fmt::Formatter, alt: &Alt<T>) -> fmt::Result
        where
            T: std::fmt::Display + std::fmt::Debug,
        {
            write!(f, "(")?;
            alt.0.fmt(f)?;
            write!(f, "|")?;

            let mut c = &*alt.1;
            loop {
                let p = match c {
                    Language::Alt(nalt) => {
                        nalt.0.fmt(f)?;
                        write!(f, "|");
                        &nalt.1
                    }
                    _ => {
                        c.fmt(f)?;
                        write!(f, ")");
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

impl<T> fmt::Display for Cat<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        write!(f, "{}", self.1)
    }
}

impl<T> fmt::Display for Repeat<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            Language::LToken(_) | Language::Alt(_) => {
                self.0.fmt(f)?;
                write!(f, "*")
            }
            _ => {
                write!(f, "(")?;
                self.0.fmt(f)?;
                write!(f, ")*")
            }
        }
    }
}

impl<T> fmt::Display for Language<T>
where
    T: std::fmt::Display + std::fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Language::Epsilon => write!(f, "ε"),
            Language::LToken(ref c) => write!(f, "{}", c),
            Language::Alt(ref alt) => write!(f, "{}", alt),
            Language::Cat(ref cat) => write!(f, "{}", cat),
            Language::Repeat(ref rep) => write!(f, "{}", rep),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_patterns() {
        let pattern = tok('A');
        assert_eq!(pattern, Language::LToken(Token('A')));
    }

    #[test]
    fn display_1() {
        let pattern = tok('A');
        assert_eq!(format!("{}", pattern), "A");
    }

    #[test]
    fn display_2() {
        let pattern = cat(tok('A'), tok('B'));
        assert_eq!(format!("{}", pattern), "AB");
    }

    #[test]
    fn display_macro_1() {
        let pattern = cat!(tok('a'), tok('b'));
        assert_eq!(format!("{}", pattern), "ab");
    }

    #[test]
    fn display_alt_1() {
        let pattern = alt(tok('a'), alt(tok('b'), tok('c')));
        println!("{:?}", pattern);
        assert_eq!(format!("{}", pattern), "(a|b|c)");
    }
    
    #[test]
    fn complex_pattern() {
        let mut pattern = alt(cat(tok('f'), cat(tok('o'), tok('o'))),
                              alt(cat(tok('b'), cat(tok('a'), tok('r'))),
                                  cat(tok('b'), cat(tok('a'), tok('z')))));
        assert_eq!(format!("{}", pattern), "(foo|bar|baz)");
    }

    #[test]
    fn complex_macro_pattern() {
        let mut pattern = alt!(cat!(tok('f'), tok('o'), tok('o')),
                               cat!(tok('b'), tok('a'), tok('r')),
                               cat!(tok('b'), tok('a'), tok('z')));
        assert_eq!(format!("{}", pattern), "(foo|bar|baz)");
    }
    
}
