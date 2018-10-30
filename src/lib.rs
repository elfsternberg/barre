extern crate rand;
use std::fmt;
use std::iter::FromIterator;
use std::iter::Peekable;
use std::collections::HashMap;

type NodeId = usize;

/// A language of <T> recognizes a few different things:
///    Empty: the Empty expression, which matches no strings at all
///    Epsilon: the Empty string, which matches... the empty string, of course!
///    Obj, which matches a single object (for example, a character)
///    Cat, which matches concatenations of the language (one character following another)
///    Alt, which matches alternatives between languages (|)
///    Repeat, which matches the language repeated  (*)
///
///    Eventually, I hope to add specializations such as Any (which is a specialization of Cat),
///    (+), which is the Cat of a Char and a Repeat, and from there... who knows?
///
/// Fortunately, languages are small and cheap, in this implementation, consisting at most
/// of a single tuple.

#[derive(Debug, Clone, PartialEq)]
pub struct Range<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    pub ranges: Vec<(T, T)>,
}

impl<T> Range<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    pub fn new(pairs: &[(T, T)]) -> Range<T> {
        Range {
            ranges: Vec::from_iter(pairs.iter().map(|ref p| {
                if p.1 < p.0 {
                    (p.1.clone(), p.0.clone())
                } else {
                    (p.0.clone(), p.1.clone())
                }
            })),
        }
    }

    pub fn has(&self, t: &T) -> bool {
        self.ranges.iter().any(|ref p| *t >= p.0 && *t <= p.1)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Language<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    Empty,
    Epsilon,
    Token(T),
    Range(Range<T>),
    NRange(Range<T>),
    Any,
    Alt(NodeId, NodeId),
    Cat(NodeId, NodeId),
    Repeat(NodeId),
}

/// Given an expression, recognize if the string matches the expression.
///
/// Traditionally, regular expressions emerged from *generative*
/// descriptions, that is, the expression engine described a way of
/// *generating* a series of strings, and then the trick was to ask,
/// "Could this string be generated by this generator?"
///
/// We're going straight for a simple recognizer.  It's a shame that
/// "recognizer" is one of those steps in learning about parsing
/// that's never covered in much depth.
///
/// For our purpose, a recognizer is a vector of language atoms.
/// Language atoms that refer to other language atoms use the index of
/// those atoms to look them up.  Once a recognizer has been built,
/// it's starting point is the LAST item pushed into the arena.

#[derive(Clone)]
pub struct Recognizer<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    language: Vec<Language<T>>,
    memo: HashMap<(NodeId, T), NodeId>,
    start: Option<usize>,
    empty: usize,
    epsilon: usize,
}

/// The implementation of this is a vector of Language items.

impl<T> Recognizer<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    /// By default, a Recognizer recognizes only the Empty Language,
    /// i.e.  *no* strings can be recognized.
    pub fn new() -> Recognizer<T> {
        // Currently, this recognizer recognizes no strings of tokens
        let mut lang = Recognizer::<T> {
            language: Vec::new(),
            memo: HashMap::new(),
            start: None,
            epsilon: 0,
            empty: 0,
        };
        lang.language.push(Language::Epsilon);
        lang.epsilon = lang.language.len() - 1;
        lang.language.push(Language::Empty);
        lang.empty = lang.language.len() - 1;
        lang
    }

    pub fn len(&self) -> usize {
        self.language.len()
    }

    fn lastpos(&self) -> usize {
        self.len() - 1
    }

    /// Push a token into the recognizer, returning its index.
    pub fn tok(&mut self, t: T) -> usize {
        self.language.push(Language::Token(t));
        self.lastpos()
    }

    /// Push a token into the recognizer, returning its index.
    pub fn ext(&mut self, s: T, e: T) -> usize {
        self.language.push(Language::Range(Range::new(&[(s, e)])));
        self.lastpos()
    }

    /// Push an alternator into the recognizer, returning its index.
    pub fn alt(&mut self, l: usize, r: usize) -> usize {
        if self.is_empty(l) {
            r
        } else if self.is_empty(r) {
            l
        } else {
            self.language.push(Language::Alt(l, r));
            self.lastpos()
        }
    }

    pub fn cat(&mut self, l: usize, r: usize) -> usize {
        if self.is_empty(l) || self.is_empty(r) {
            self.empty
        } else {
            self.language.push(Language::Cat(l, r));
            self.lastpos()
        }
    }

    pub fn rep(&mut self, n: usize) -> usize {
        self.language.push(Language::Repeat(n));
        self.lastpos()
    }

    pub fn plus(&mut self, n: usize) -> usize {
        let t = self.rep(n);
        self.cat(n, t)
    }

    pub fn any(&mut self) -> usize {
        self.language.push(Language::Any);
        self.lastpos()
    }

    pub fn que(&mut self, n: usize) -> usize {
        let epsilon = self.epsilon;
        self.alt(n, epsilon) // This or epsilon
    }

    /// This is the function that determines if it's possible for the
    /// current Language to be nullable (that is, it can return the
    /// empty string).  If it can, then matching can continue, or the
    /// result is true; otherwise false.
    fn nullable(&self, i: usize) -> bool {
        match self.language[i] {
            Language::Empty => false,
            Language::Epsilon => true,
            Language::Token(_) => false,
            Language::Any => false,
            Language::Range(_) => false,
            Language::NRange(_) => false,
            Language::Alt(l, r) => self.nullable(l) || self.nullable(r),
            Language::Cat(l, r) => self.nullable(l) && self.nullable(r),
            Language::Repeat(_) => true,
        }
    }

    fn is_empty(&self, c: usize) -> bool { c == self.empty }

    fn is_epsilon(&self, c: usize) -> bool { c == self.epsilon }

    /// Given a step in the recognizer, finds the derivative of the
    /// current step after any symbols have been considered.
    fn derive(&mut self, c: &T, p: usize) -> usize {
        // println!("{} {} {:?}", p, c, self);

        if let Some(nodeid) = self.memo.get(&(p, c.clone())) {
            println!("Memo used.");
            return *nodeid;
        };
        
        let respect_derivative = match self.language[p].clone() {
            // Dc(∅) = ∅
            Language::Empty => self.empty,

            // Dc(ε) = ∅
            Language::Epsilon => self.empty,

            // Dc(c) = ε, always:
            Language::Any => self.epsilon,

            Language::Range(ref r) => {
                if r.has(c) {
                    self.epsilon
                } else {
                    self.empty
                }
            }

            Language::NRange(ref r) => {
                if r.has(c) {
                    self.empty
                } else {
                    self.epsilon
                }
            }

            // Dc(c) = ε if c = c'
            // Dc(c') = ∅ if c ≠ c'
            Language::Token(ref d) => {
                if *d == *c {
                    self.epsilon
                } else {
                    self.empty
                }
            }

            // Dc(re1 | re2) = Dc(re1) | Dc(re2)
            Language::Alt(l, r) => {
                let dl = self.derive(c, l);
                let dr = self.derive(c, r);
                // println!("Alt: {:?} {:?}", dl, dr);
                self.alt(dl, dr)
            }

            // Dc(L ○ R) = Dc(L) ○ R if L does not contain the empty string
            // Dc(L ○ R) = Dc(L) ○ R ∪ Dc(R) if L contains the empty string
            Language::Cat(l, r) => {
                let nld = self.derive(c, l);
                let lhs = self.cat(nld, r);
                if self.nullable(l) {
                    let rhs = self.derive(c, r);
                    self.alt(lhs, rhs)
                } else {
                    lhs
                }
            }

            // Dc(re*) = Dc(re) re*
            Language::Repeat(n) => {
                let derived = self.derive(c, n);
                self.cat(derived, p)
            }
        };
        self.memo.insert((p, c.clone()), respect_derivative);
        respect_derivative
    }

    fn scanner<I>(&mut self, items: &mut Peekable<I>, ipos: usize) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut pos = ipos;
        loop {
            match items.next() {
                // If there is no next item and we are at a place where the empty string
                // (Epsilon, not the empty pattern!) *could* be a valid match, return
                // true.
                None => break self.nullable(pos),
                
                Some(ref c) => {
                    let np = self.derive(c, pos);
                    let nl = self.language[np].clone();
                    match nl {
                        Language::Empty => break false,
                        Language::Epsilon => match items.peek() {
                            Some(_) => break false,
                            None => break true,
                        },
                        // Essentially, for all other possibilities, we
                        // just need to recurse across our nodes until
                        // we hit Empty or Epsilon, and then we're
                        // done.
                        _ => { pos = np; }
                    }
                }
            }
        }
    }
    
    fn scan<I>(&mut self, items: &mut I) -> bool 
    where
        I: Iterator<Item = T>,
    {
        let mut items = items.peekable();
        let start = match self.start {
            None => {
                let n = self.lastpos();
                self.start = Some(n);
                n
            }
            Some(n) => n,
        };
        self.scanner(&mut items, start)
    }

    pub fn recognize<I>(&mut self, items: &mut I) -> bool
    where
        I: Iterator<Item = T>,
    {
        let mut runner = self.clone();
        runner.scan(items)
    }

}

impl<T> fmt::Debug for Recognizer<T>
where
    T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_helper<T>(f: &mut fmt::Formatter, language: &[Language<T>], p: usize) -> fmt::Result
        where
            T: std::clone::Clone + std::cmp::PartialEq + std::fmt::Debug + std::fmt::Display + std::cmp::Ord + std::hash::Hash,
        {
            match language[p] {
                Language::Empty => write!(f, "⊘")?,
                Language::Epsilon => write!(f, "ε")?,
                Language::Token(ref i) => write!(f, "{}", i)?,
                Language::Any => write!(f, ".")?,
                Language::Range(ref r) => {
                    write!(f, "[");
                    for pair in r.ranges.iter() {
                        write!(f, "{:?}-{:?}", pair.0, pair.1);
                    }
                    write!(f, "]");
                }
                Language::NRange(ref r) => {
                    write!(f, "[^");
                    for pair in r.ranges.iter() {
                        write!(f, "{:?}-{:?}", pair.0, pair.1);
                    }
                    write!(f, "]");
                }
                Language::Alt(l, r) => {
                    write!(f, "(")?;
                    fmt_helper(f, language, l)?;
                    write!(f, "|")?;
                    let mut c = r;
                    loop {
                        let p = match language[c] {
                            Language::Alt(l, r) => {
                                fmt_helper(f, language, l)?;
                                write!(f, "|");
                                r
                            }
                            _ => {
                                fmt_helper(f, language, r)?;
                                write!(f, ")");
                                break;
                            }
                        };
                        c = p;
                    }
                    write!(f, "")?;
                }
                Language::Cat(l, r) => {
                    fmt_helper(f, language, l)?;
                    fmt_helper(f, language, r)?;
                }
                Language::Repeat(n) => {
                    write!(f, "(")?;
                    fmt_helper(f, language, n)?;
                    write!(f, ")*")?;
                }
            }
            write!(f, "")
        }

        let start = self.language.len() - 1;
        fmt_helper(f, &self.language, start)
    }
}

macro_rules! re {
    () => { re!{ char; } };

    ( $sty:ty ; ) => { Recognizer::<$sty>::new() };

    (@process $pt:ident, plus { $iop:ident { $($iex:tt)+ } }) => {
        {
            let n = re!(@process $pt, $iop { $($iex)* });
            $pt.plus(n)
        }
    };

    (@process $pt:ident, rep { $iop:ident { $($iex:tt)+ } }) => {
        {
            let n = re!(@process $pt, $iop { $($iex)* });
            $pt.rep(n)
        }
    };

    (@process $pt:ident, que { $iop:ident $($iex:tt)+ }) => {
        {
            let n = re!(@process $pt, $iop { $($iex)* });
            $pt.que(n)
        }
    };

    // Terminator
    (@process $pt:ident, ext { $s:expr ; $e:expr }) => {
        $pt.ext($s, $e)
    };

    // Terminator
    (@process $pt:ident, any { $e:expr }) => {
        {
            $pt.any()
        }
    };

    // Terminator
    (@process $pt:ident, tok { $e:expr }) => {
        {
            $pt.tok($e)
        }
    };

    // cat { tok { 'A' }, tok { 'B' } }
    (@process $pt:ident, $op:ident { $lop:ident { $($lex:tt)+ }, $rop:ident { $($rex:tt)+ } }) => {
        {
            let l = re!(@process $pt, $lop { $($lex)* });
            let r = re!(@process $pt, $rop { $($rex)* });
            $pt.$op(l, r)
        }
    };

    // cat { tok { 'A' }, tok { 'B' }, tok { 'C' } }
    (@process $pt:ident, $op:ident { $lop:ident { $($lex:tt)+ }, $rop:ident { $($rex:tt)+ }, $($xex:tt)+ }) => {
        {
            let l = re!(@process $pt, $lop { $($lex)* });
            let r = re!(@process $pt, $op { $rop { $($rex)* }, $($xex)* });
            $pt.$op(l, r)
        }
    };

    ($sty:ty; $($cmds:tt)+) => {
        {
            let mut pt = Recognizer::<$sty>::new();
            {
                let _ = re!(@process pt, $($cmds)*);
            }
            pt
        }
    };

    ($($cmds:tt)+) => { re!{ char; $($cmds)* } };
}

#[cfg(test)]
mod tests {

    use rand::distributions::Uniform;
    use rand::{thread_rng, Rng};
    use std::time::Instant;

    use crate::Recognizer;

    macro_rules! mkpair {
        ($(($l:expr, $r:expr)),*) => {
            vec![$(($l, $r)),*]
            .into_iter()
            .map(|pair| { (String::from(pair.0), pair.1) });
        }
    }

    macro_rules! testpat {
        ($pt:ident; [$(($l:expr, $r:expr)),*]) => {
            {
                let matches = mkpair![$(($l, $r)),*];
                for pair in matches {
                    println!("{:?} {:?}", &pair.0, &pair.1);
                    assert!($pt.recognize(&mut pair.0.chars()) == pair.1);
                }
            }
        }
    }

    #[test]
    fn it_works() {
        let mut pattern = Recognizer::<char>::new();
        assert!(pattern.nullable(1) == false);
        assert!(pattern.nullable(0) == true);

        pattern.tok('A');
        assert!(pattern.recognize(&mut String::from("AA").chars()) == false);
        assert!(pattern.nullable(2) == false);
        assert!(pattern.recognize(&mut String::from("A").chars()) == true);
        assert!(pattern.recognize(&mut String::from("B").chars()) == false);
        assert!(pattern.recognize(&mut String::from("").chars()) == false);
    }

    #[test]
    fn alt_matches() {
        let mut pattern = Recognizer::<char>::new();
        let l = pattern.tok('A');
        let r = pattern.tok('B');
        let altpattern = pattern.alt(l, r);
        assert!(pattern.nullable(altpattern) == false);

        assert!(pattern.recognize(&mut String::from("A").chars()) == true);
        assert!(pattern.recognize(&mut String::from("B").chars()) == true);
        assert!(pattern.recognize(&mut String::from("C").chars()) == false);
    }

    #[test]
    fn cat_matches() {
        let mut pattern = Recognizer::<char>::new();
        let a = pattern.tok('A');
        let b = pattern.tok('B');
        let c = pattern.tok('C');
        let d = pattern.tok('D');
        let e = pattern.cat(c, d);
        let f = pattern.cat(b, e);
        let _ = pattern.cat(a, f);
        assert!(pattern.recognize(&mut String::from("ABCD").chars()) == true);
        assert!(pattern.recognize(&mut String::from("ACBD").chars()) == false);
    }

    #[test]
    fn can_display() {
        let mut pattern = Recognizer::<char>::new();
        let a = pattern.tok('A');
        let b = pattern.tok('B');
        let f = pattern.cat(a, b);
        let g = pattern.rep(f);
        let h = pattern.tok('C');
        let _ = pattern.cat(g, h);
        assert!(format!("{:?}", pattern) == "(AB)*C");
    }

    #[test]
    fn repeat_matches() {
        let mut pattern = Recognizer::<char>::new();
        let a = pattern.tok('A');
        let b = pattern.tok('B');
        let f = pattern.cat(a, b);
        let g = pattern.rep(f);
        let h = pattern.tok('C');
        let _ = pattern.cat(g, h);

        assert!(pattern.recognize(&mut String::from("ABC").chars()) == true);
        assert!(pattern.recognize(&mut String::from("ABABC").chars()) == true);
        assert!(pattern.recognize(&mut String::from("ABABABABABC").chars()) == true);
        assert!(pattern.recognize(&mut String::from("ABAC").chars()) == false);
        assert!(pattern.recognize(&mut String::from("C").chars()) == true);
    }

    #[test]
    fn simple_token_macro() {
        let mut pattern = re!{char; tok { 'A' } };
        testpat!(pattern; [("A", true), ("AA", false), ("B", false), ("", false)]);
    }

    /* Important, because this confused me: The repeat operator is zero
       or more, so the empty string is valid here.
     */

    #[test]
    fn simple_repeat_macro() {
        let mut pattern = re!{char; rep { tok { 'A' } } };
        testpat!(pattern; [
            ("A", true), ("AA", true), ("B", false), ("", true), ("AAAAA", true),
            ("AAAAB", false), ("BAAAA", false)
        ]);
    }

    #[test]
    fn repeat_macro() {
        let mut pattern = re!{char; rep { tok { 'A' } } };
        testpat!(pattern; [
            ("A", true), ("AA", true), ("B", false), ("", true), ("AAAAA", true),
            ("AAAAB", false), ("BAAAA", false)
        ]);
    }

    #[test]
    fn simple_cat_macro() {
        let mut pattern = re!{char; cat { tok { 'A' }, tok { 'B' } } };
        testpat!(pattern; [
            ("AB", true), ("A", false), ("B", false), ("AA", false), ("BB", false),
            ("AA ", false), ("", false), ("AAB", false)
        ]);
    }

    #[test]
    fn extended_cat_macro() {
        let mut pattern = re!{char; cat { tok { 'A' }, tok { 'B' }, tok { 'C' } } };

        let matches = mkpair![("ABC", true), ("A", false), ("B", false), ("AAC", false), ("BBA", false),
                              ("AA ", false), ("", false), ("AAB", false)];

        let mut testlen: Option<usize> = None;

        for pair in matches {
            let maxlen = if let Some(c) = testlen { c } else {
                testlen = Some(pattern.len());
                pattern.len()
            };
            println!("{:?} {:?}", &pair.0, &pair.1);
            assert!(pattern.recognize(&mut pair.0.chars()) == pair.1);
            assert!(pattern.len() == maxlen);
        }
    }

    #[test]
    fn mildly_complex_macro() {
        // /AB(CC|DDDD)E*F/
        let mut pattern = re!{char; cat { tok { 'A' }, tok { 'B' },
        alt { cat { tok { 'C' }, tok { 'C' } },
              cat { tok { 'D' }, tok { 'D' }, tok { 'D' }, tok { 'D' } } },
        rep { tok { 'E' } }, tok { 'F' } } };
        testpat!(pattern; [
            ("ABCCF", true), ("ABCCEF", true), ("ABCCEEEEEEEF", true), ("ABDDDDF", true),
            ("ABDDDDEF", true), ("ABDDDDEEEEEEF", true), ("AB", false), ("ABCEF", false),
            ("ABCDEF", false), ("ABCCEFF", false), ("", false), ("ABCDDF", false)
        ]);
    }

    #[test]
    fn mildly_complex_untyped_macro() {
        // /AB(CC|DDDD)E*F/
        let mut pattern = re!{cat { tok { 'A' }, tok { 'B' },
        alt { cat { tok { 'C' }, tok { 'C' } },
              cat { tok { 'D' }, tok { 'D' }, tok { 'D' }, tok { 'D' } } },
        rep { tok { 'E' } }, tok { 'F' } } };
        testpat!(pattern; [
            ("ABCCF", true), ("ABCCEF", true), ("ABCCEEEEEEEF", true), ("ABDDDDF", true),
            ("ABDDDDEF", true), ("ABDDDDEEEEEEF", true), ("AB", false), ("ABCEF", false),
            ("ABCDEF", false), ("ABCCEFF", false), ("", false), ("ABCDDF", false)
        ]);
    }

    #[test]
    fn empty_untyped_macro() {
        let pattern = re!{};
        assert!(pattern.len() == 2);
    }

    #[test]
    fn empty_typed_macro() {
        let pattern = re!{char;};
        assert!(pattern.len() == 2);
    }

    #[test]
    fn plus_pattern() {
        let mut pattern = re!{char; plus { tok { 'A' } } };
        testpat!(pattern; [
            ("", false), ("A", true), ("AA", true), ("B", false), ("BA", false), ("AAB", false)]);
    }

    #[test]
    fn any_pattern() {
        let mut pattern = re!{char; cat { tok { 'A' }, any { '.' }, tok { 'B' } } };
        testpat!(pattern; [
            ("", false), ("ACB", true), ("A.B", true), ("AλB", true), ("AB", false), ("BA", false),
            ("AEBF", false)]);
    }

    #[test]
    fn leading_any() {
        let mut pattern = re!{char; cat { rep { any { '.' } }, tok { 'B' }, tok { 'C' } } };
        testpat!(pattern; [
            ("", false), ("BC", true), ("ZZZZZABC", true), ("λBC", true), ("CB", false), ("B", false),
            ("AEBF", false)]);
    }

    #[test]
    fn que_pattern() {
        let mut pattern = re!{char; cat { tok { 'A' }, que { tok { 'C' } }, tok { 'B' } } };
        testpat!(pattern; [
            ("", false), ("ACB", true), ("AB", true), ("A.B", false), ("AC", false), ("BA", false),
            ("AEBF", false)]);
    }

    fn distribution(max: usize) -> (usize, usize) {
        let mut rng = thread_rng();
        let dst = Uniform::new_inclusive(1, max);
        let x1 = rng.sample(dst);
        (x1, max - x1)
    }

    #[test]
    fn que_expn_pattern() {
        let mut pattern = re!{char; cat { rep { any { '.' } },
                                          que { tok { 'A' } },
                                          que { tok { 'A' } },
                                          que { tok { 'A' } },
                                          tok { 'A' },
                                          tok { 'A' },
                                          tok { 'A' },
                                          rep { any { '.' } }
        } };

        let mut rng = thread_rng();
        let dst = Uniform::new_inclusive(3, 6);
        for _ in 0..10 {
            let sz = rng.sample(dst);
            let sr = distribution(29 - sz);
            let mut sample = "X".repeat(sr.0);
            sample.push_str(&"A".repeat(sz));
            sample.push_str(&"Z".repeat(sr.1));

            let now = Instant::now();
            let result = pattern.recognize(&mut sample.chars());
            let duration = now.elapsed();
            println!(
                "Taken: {}.{:04}",
                duration.as_secs(),
                duration.subsec_millis()
            );
            println!("String: {}", sample);
            assert!(result == true);
        }
    }

    #[test]
    fn ext_pattern() {
        let mut pattern = re!{char; cat{ tok { 'A' }, ext{ 'B' ; 'E' }, tok{ 'F' } } };
        println!("{:?}", pattern);
        testpat!(pattern; [
            ("", false), ("AF", false), ("ABF", true), ("ADF", true), ("ACF", true), ("ACD", false),
            ("ABC", false), ("ABFG", false)]);
    }
}
