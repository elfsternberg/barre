#![cfg_attr(feature = "cargo-clippy", allow(redundant_field_names))]

extern crate arena;

// #[cfg(feature = "render_trees")]
// extern crate dot;

pub mod types;

#[macro_use]
pub mod language;

#[macro_use]
pub mod grammar;

pub mod barre;
pub use grammar::{Grammar, ParseTree};

pub use barre::Barre;

mod builder;

pub mod render;
