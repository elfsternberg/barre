use grammar::{Grammar, Deriver};
use hashbrown::HashSet;
use language::Language;
use parsesets::ParseTree;

pub struct Barre(Grammar);

impl Barre {
    pub fn from_language(lang: &Language) -> Barre {
        Barre(Grammar::from_language(lang))
    }

    pub fn new() -> Barre {
        Barre(Grammar::new())
    }

    pub fn from_grammar(gram: Grammar) -> Barre {
        Barre(gram)
    }
        

    pub fn parse<I>(&mut self, items: &mut I) -> Option<HashSet<ParseTree>>
    where
        I: Iterator<Item = char>,
    {
        let mut deriver = Deriver::new(&self.0);
        match deriver.parse(items) {
            Some(parseset) => Some(parseset.0),
            None => None,
        }
    }
}

impl Default for Barre {
    fn default() -> Self {
        Self::new()
    }
}
