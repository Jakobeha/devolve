#![doc = include_str!("../README.md")]
#![feature(decl_macro)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![feature(new_uninit)]
#![feature(drain_filter)]
#![feature(hash_set_entry)]
#![feature(assert_matches)]
#![feature(generic_associated_types)]
#![feature(associated_type_defaults)]

pub use graph::*;

mod graph;
/// Macros to create node types and graph functions
#[cfg(feature = "macros")]
pub mod macros;
/// Miscellaneous helpers which may go into other crates in the future
pub mod misc;

