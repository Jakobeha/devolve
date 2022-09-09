#![doc = include_str!("../README.md")]
#![feature(decl_macro)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![feature(new_uninit)]
#![feature(drain_filter)]
#![feature(hash_set_entry)]

pub use graph::*;

mod graph;
/// Miscellaneous helpers which may go into other crates in the future
pub mod misc;

