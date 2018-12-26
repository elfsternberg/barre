use grammar::{Deriver, Grammar};
use indexmap::IndexSet;
use language::Language;
use parsesets::Tree;
use siaa::{Riaa, Siaa};

/// This is the primary API for interacting with the Barre parser.  The
/// types are the input type, and the output type.  The input type
/// must be cloneable, iteratable and peekable.  The output type must
/// be cloneable and hashable.  The user typically must provide a
/// function for converting from one to the other.  In the most basic
/// form, that's just an ID function.

pub struct Barre<T: Siaa + 'static, U: Riaa<T> + std::convert::From<T> + 'static>(Grammar<T, U>);

impl<T: Siaa, U: Riaa<T>> Barre<T, U>
    where U: std::convert::From<T>
{
    pub fn from_language(lang: &Language<T>) -> Barre<T, U> {
        Barre(Grammar::from_language(lang))
    }

    pub fn new() -> Barre<T, U> {
        Barre(Grammar::new())
    }

    pub fn from_grammar(gram: Grammar<T, U>) -> Barre<T, U> {
        Barre(gram)
    }

    pub fn parse<I>(&mut self, items: &mut I) -> Option<IndexSet<Tree<U>>>
    where
        I: Iterator<Item = T>,
    {
        let mut deriver = Deriver::new(&self.0);
        match deriver.parse(items) {
            Some(parseset) => Some(parseset.0),
            None => None,
        }
    }
}

impl<T: Siaa, U: Riaa<T>> Default for Barre<T, U>
    where U: std::convert::From<T>
{
    fn default() -> Self {
        Self::new()
    }
}
