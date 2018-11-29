use arena::{Arena, NodeId};
use std::collections::{HashMap, HashSet};

use builder::{init_barre_arena, language_to_arena};
use grammar::{Grammar, ParseTree};
use language::Language;
use types::{Parser, Siaa};

pub struct Barre<T: Siaa> {
    arena: Arena<Parser<T>>,
    start: NodeId,
    empty: NodeId,
}

impl<T: Siaa> Barre<T> {
    pub fn from_arena(arena: Arena<Parser<T>>, start: NodeId) -> Barre<T> {
        Barre::<T> {
            arena: arena,
            start: start,
            empty: 1,
        }
    }

    pub fn from_language(lang: &Language<T>) -> Barre<T> {
        let arena_start = language_to_arena(lang);
        Barre::from_arena(arena_start.0, arena_start.1)
    }

    pub fn new() -> Barre<T> {
        Barre::from_arena(init_barre_arena(), 1)
    }

    pub fn parse<I>(&mut self, items: &mut I) -> Option<HashSet<ParseTree<T>>>
    where
        I: Iterator<Item = T>,
    {
        let mut grammar = Grammar {
            arena: self.arena.clone(),
            memo: HashMap::new(),
            empty: self.empty,
        };

        grammar.parse(items, self.start)
    }
}

impl<T:Siaa> Default for Barre<T> {
    fn default() -> Self {
        Self::new()
    }
}
