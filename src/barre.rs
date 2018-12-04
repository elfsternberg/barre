use arena::{Arena, NodeId};
use std::collections::{HashMap, HashSet};

use builder::{init_barre_arena, language_to_arena};
use grammar::{Grammar, ParseTree};
use language::Language;
use types::Parser;

pub struct Barre {
    arena: Arena<Parser>,
    start: NodeId,
    empty: NodeId,
}

impl Barre {
    pub fn from_arena(arena: Arena<Parser>, start: NodeId) -> Barre {
        Barre {
            arena: arena,
            start: start,
            empty: 1,
        }
    }

    pub fn from_language(lang: &Language) -> Barre {
        let arena_start = language_to_arena(lang);
        Barre::from_arena(arena_start.0, arena_start.1)
    }

    pub fn new() -> Barre {
        Barre::from_arena(init_barre_arena(), 1)
    }

    pub fn parse<I>(&mut self, items: &mut I) -> Option<HashSet<ParseTree>>
    where
        I: Iterator<Item = char>,
    {
        let mut grammar = Grammar {
            arena: self.arena.clone(),
            store: vec!(),
            memo: HashMap::new(),
            empty: self.empty,
        };

        grammar.parse(items, self.start)
    }
}

impl Default for Barre {
    fn default() -> Self {
        Self::new()
    }
}
