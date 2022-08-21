#![doc = include_str!("../README.md")]
#![feature(decl_macro)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![feature(new_uninit)]
#![feature(drain_filter)]

pub use ctx::*;
pub use graph::*;

mod ctx;
mod graph;
/// Miscellaneous helpers which may go into other crates in the future
pub mod misc;
/// Defines the DSL type system and provides reflection capabilities to interop with Rust's types.
pub mod rust_type;

