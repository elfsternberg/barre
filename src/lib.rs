#![cfg_attr(feature = "cargo-clippy", allow(redundant_field_names))]

extern crate arena;
extern crate consy;

// #[cfg(feature = "render_trees")]
// extern crate dot;

pub mod types;

#[macro_use]
pub mod language;

mod parsesets;
pub use parsesets::ParseTree;

#[macro_use]
pub mod grammar;

pub mod barre;
pub use grammar::Grammar;

pub use barre::Barre;

mod builder;

pub mod render;
