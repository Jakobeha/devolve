use crate::rust_type::{IntrinsicRustType, RustType};
use crate::rust_type::primitive::PrimitiveType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeStructure {
    Opaque,
    Primitive(PrimitiveType),
    CReprEnum { variants: Vec<TypeEnumVariant> },
    CReprStruct { body: TypeStructBody },
    /// Note: these are "technically" not actual tuples, as tuples in Rust have no defined repr.
    /// Thus in order to use them in Rust, you must either assume C-style repr or coerce to a C-repr struct.
    CTuple { elements: Vec<RustType> },
    Array { elem: Box<RustType>, length: usize },
    Pointer { referenced: IntrinsicRustType },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnumVariant {
    pub variant_name: String,
    pub body: TypeStructBody
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeStructBody {
    None,
    Tuple(Vec<RustType>),
    Fields(Vec<TypeStructField>)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeStructBodyForm {
    None,
    Tuple,
    Fields
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStructField {
    pub name: String,
    pub rust_type: RustType
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IsSubtypeOf {
    No,
    Unknown,
    Yes,
}

impl TypeStructure {
    pub fn array_elem_type_and_length(&self) -> Option<(&RustType, usize)> {
        match self {
            TypeStructure::Array { elem, length } => Some((elem, *length)),
            _ => None
        }
    }

    pub fn tuple_elem_types(&self) -> Option<&Vec<RustType>> {
        match self {
            TypeStructure::CTuple { elements } => Some(elements),
            _ => None
        }
    }

    pub fn tuple_struct_tuple_item_types(&self) -> Option<&Vec<RustType>> {
        match self {
            TypeStructure::CReprStruct { body: TypeStructBody::Tuple(tuple_items) } => Some(tuple_items),
            _ => None
        }
    }

    pub fn field_struct_field_types(&self) -> Option<&Vec<TypeStructField>> {
        match self {
            TypeStructure::CReprStruct { body: TypeStructBody::Fields(fields) } => Some(fields),
            _ => None
        }
    }
}

impl TypeStructBody {
    pub fn form(&self) -> TypeStructBodyForm {
        match self {
            TypeStructBody::None => TypeStructBodyForm::None,
            TypeStructBody::Tuple(_) => TypeStructBodyForm::Tuple,
            TypeStructBody::Fields(_) => TypeStructBodyForm::Fields
        }
    }
}

impl Default for TypeStructBody {
    fn default() -> Self {
        TypeStructBody::None
    }
}
