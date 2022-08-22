use std::any::TypeId;

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
            structure: TypeStructure::Opaque
        }
    }

    pub fn from_intrinsic(type_name: RustTypeName, data: IntrinsicRustType) -> Self {
        RustType {
            type_id: Some(data.type_id),
            type_name,
            size: data.size,
            align: data.align,
            structure: TypeStructure::Opaque
        }
    }

    pub fn display<'a, 'b>(&'a self, snis: &'b SimpleNamesInScope) -> RustTypeNameDisplay<'a, 'b> {
        self.type_name.display(snis)
    }
}

/// Considered equal if both types have the same type id or name
impl PartialEq for RustType {
    fn eq(&self, other: &RustType) -> bool {
        if self.type_id.is_some() && other.type_id.is_some() {
            self.type_id == other.type_id
        } else {
            self.type_name == other.type_name
        }
    }
}

impl Eq for RustType {}