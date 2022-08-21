use std::any::TypeId;
use std::collections::HashSet;
use std::fmt::Display;
use join_lazy_fmt::Join;
use structure::TypeStructure;
use type_name::RustTypeName;

mod type_name;
mod structure;
mod primitive;
mod registry;
mod subtype;
mod size_align;
mod intrinsic;

pub use type_name::*;
pub use structure::*;
pub use primitive::*;
pub use registry::*;
pub use subtype::*;
pub use size_align::*;
pub use intrinsic::*;

/// A Rust type which can be used by the DSL:
/// either a type defined in the DSL or a type interoperable with the DSL.
#[derive(Debug, Clone)]
pub struct RustType {
    /// Corresponds to [TypeId::of]. DSL types will have no type_id.
    pub type_id: Option<TypeId>,
    /// Corresponds to [std::any::type_name] but more detailed.
    pub type_name: RustTypeName,
    /// Corresponds to [std::size_of]
    pub size: usize,
    /// Corresponds to [std::align_of]
    pub align: usize,
    /// Structure
    pub structure: TypeStructure
}

impl RustType {
    pub fn unknown() -> Self {
        RustType {
            type_id: None,
            type_name: RustTypeName::unknown(),
            size: 0,
            align: 0,
            structure: TypeStructure::Opaque
        }
    }

    pub fn bottom() -> Self {
        RustType {
            type_id: None,
            type_name: RustTypeName::bottom(),
            size: 0,
            align: 0,
            structure: TypeStructure::Bottom
        }
    }

    pub fn display(&self, simple_names_in_scope: &HashSet<String>) -> RustTypeNameUnambiguous<'_> {
        self.type_name.display(simple_names_in_scope)
    }
}