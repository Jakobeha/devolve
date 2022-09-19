#![feature(iterator_try_collect)]
#![feature(decl_macro)]

use proc_macro::TokenStream;
use syn::parse_macro_input;
pub use graph_macro::*;

mod node_type_attribute;
mod graph_macro;

/// Generates a `NodeTypeFn` for the corresponding function, named `<function_name>__node_type`.
///
/// - If the output is a result, any error will be a
/// - `#[node_type(no_ctx)]`: The function does not take a `&mut RuntimeCtx` as its first argument.
/// - `#[node_type(default)]` or `#[node_type(default = "expression")]` on attribute: Give the attribute `Default::default()` or `expression` as the default value.
/// - `#[node_type(validator)]` on a nested function: Calls the function with compile-time info and fails before compiling if it returns `Err`
///
/// # Example
///
/// ```rust
/// use dui_graph::macros::node_type;
/// use structural_reflection::derive::{HasTypeName, HasStructure};
/// use structural_reflection::RustType;
///
/// struct MyRuntimeCtx;
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// struct Vector3<T> { x: T, y: T, z: T }
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
/// enum DistanceType { Euclidean, Manhattan }
///
/// struct ExpectedAtLeast2Vectors;
///
/// #[node_type]
/// fn max_distance(
///     ctx: &mut MyRuntimeCtx,
///     vectors: &[Vector3<f32>],
///     #[node_type(default = "DistanceType::Euclidean")] distance_type: DistanceType
/// ) -> f32 {
///     #[node_type(validator)]
///     fn validate_inputs(
///         vectors: &[RustType],
///     ) -> Result<(), ExpectedAtLeast2Vectors> {
///         if vectors.len() < 2 {
///            return Err(ExpectedAtLeast2Vectors);
///         }
///         Ok(())
///     }
///     let offset_vectors = vectors.windows(2).map(|v| Vector3 { x: v[1].x - v[0].x, y: v[1].y - v[0].y, z: v[1].z - v[0].z });
///     let distances = match distance_type {
///        DistanceType::Euclidean => offset_vectors.map(|v| v.x * v.x + v.y * v.y + v.z * v.z),
///        DistanceType::Manhattan => offset_vectors.map(|v| v.x.abs() + v.y.abs() + v.z.abs())
///     };
///     let distance = distances.max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap().sqrt();
///     distance
/// }
/// ```
#[proc_macro_attribute]
pub fn node_type(input: TokenStream) -> TokenStream {
    parse_macro_input!(input as NodeTypeAttribute).to_token_stream()
}