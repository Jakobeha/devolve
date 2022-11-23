/// Convert data to/from the graph and compute function
pub mod raw;
/// Syntax datatype (AST) parsed from text
pub mod ast;
/// Errors for AST, IR, and lower compiling, all in one module
pub mod error;
/// Final efficient semantic datatype, lossily compiled from the IR
pub mod lower;
/// Semantic datatype, losslessly compiled from the AST
pub mod ir;
/// Static constant strings used by AST and IR (small module)
mod static_strs;

pub use static_strs::*;