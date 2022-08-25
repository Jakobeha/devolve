use std::collections::HashMap;
use std::iter::{empty, once};

pub use super::display::*;

use crate::rust_type::{RustTypeName, TypeStructBodyForm};

pub struct SerialGraph {
    pub rust_types: HashMap<String, SerialTypeDef>,
    pub nodes: HashMap<String, SerialNode>
}

pub struct SerialNode {
    pub node_type: Option<String>,
    pub input_fields: Vec<SerialFieldElem>,
    pub output_fields: Vec<SerialFieldElem>
}

pub enum SerialFieldElem {
    Header { header: String },
    Field(SerialField)
}

pub struct SerialField {
    pub name: String,
    pub rust_type: Option<SerialRustType>,
    pub value: Option<SerialValueHead>,
    pub value_children: SerialBody
}

pub enum SerialBody {
    None,
    Tuple(Vec<SerialTupleItem>),
    Fields(Vec<SerialField>)
}

pub struct SerialTupleItem {
    pub rust_type: Option<SerialRustType>,
    pub value: Option<SerialValueHead>,
    pub value_children: SerialBody
}

pub enum SerialTypeDef {
    Struct(SerialStructTypeDef),
    Enum(SerialEnumTypeDef)
}

pub struct SerialStructTypeDef {
    pub body: SerialTypeDefBody,
}

pub struct SerialEnumTypeDef {
    pub variants: Vec<SerialEnumVariantTypeDef>,
}

pub struct SerialEnumVariantTypeDef {
    pub name: String,
    pub body: SerialTypeDefBody
}

pub enum SerialTypeDefBody {
    None,
    Tuple(Vec<SerialRustType>),
    Fields(Vec<SerialFieldTypeDef>)
}

pub struct SerialFieldTypeDef {
    pub name: String,
    pub rust_type: Option<SerialRustType>,
    pub default_value: Option<SerialValueHead>,
    pub default_value_children: SerialBody
}

pub type SerialRustType = RustTypeName;

pub enum SerialValueHead {
    Integer(i64),
    Float(f64),
    String(String),
    Ref {
        node_name: String,
        field_name: String
    },
    Array(Vec<SerialValueHead>),
    Tuple(Vec<SerialValueHead>),
    Struct {
        type_name: SerialRustType,
        inline_params: Option<Vec<SerialValueHead>>,
    },
    Enum {
        type_name: SerialRustType,
        variant_name: String,
        inline_params: Option<Vec<SerialValueHead>>,
    },
}

impl SerialGraph {
    pub fn new() -> SerialGraph {
        SerialGraph {
            rust_types: HashMap::new(),
            nodes: HashMap::new()
        }
    }
}

impl SerialNode {
    pub fn new(node_type: Option<String>) -> SerialNode {
        SerialNode {
            node_type,
            input_fields: Vec::new(),
            output_fields: Vec::new()
        }
    }
}

impl SerialBody {
    pub fn form(&self) -> TypeStructBodyForm {
        match self {
            SerialBody::None => TypeStructBodyForm::None,
            SerialBody::Tuple(_) => TypeStructBodyForm::Tuple,
            SerialBody::Fields(_) => TypeStructBodyForm::Fields
        }
    }
}

impl Default for SerialBody {
    fn default() -> Self {
        SerialBody::None
    }
}

impl Default for SerialTypeDefBody {
    fn default() -> Self {
        SerialTypeDefBody::None
    }
}

// region iter rust types
impl SerialGraph {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.rust_types.values().flat_map(|t| t.iter_rust_types())
            .chain(self.nodes.values().flat_map(|n| n.iter_rust_types()))
    }
}

impl SerialTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialTypeDef::Struct(s) => Box::new(s.iter_rust_types()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialTypeDef::Enum(e) => Box::new(e.iter_rust_types()) as Box<dyn Iterator<Item=&SerialRustType>>
        }
    }
}

impl SerialStructTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.body.iter_rust_types()
    }
}

impl SerialEnumTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.variants.iter().flat_map(|v| v.body.iter_rust_types())
    }
}

impl SerialTypeDefBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialTypeDefBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialTypeDefBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter()
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialTypeDefBody::Fields(fields) => Box::new(
                fields.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
        }
    }
}

impl SerialFieldTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.rust_type.iter()
            .chain(self.default_value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.default_value_children.iter_rust_types())
    }
}

impl SerialNode {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.input_fields.iter().flat_map(|f| f.iter_rust_types())
            .chain(self.output_fields.iter().flat_map(|f| f.iter_rust_types()))
    }
}

impl SerialFieldElem {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialFieldElem::Field(field) => Box::new(field.iter_rust_types()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialFieldElem::Header { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
        }
    }
}

impl SerialValueHead {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialValueHead::Integer(_) => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Float(_) => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::String(_) => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Ref { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Tuple(items) => Box::new(
                items.iter().flat_map(|item| item.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Array(elems) => Box::new(
                elems.iter().flat_map(|elem| elem.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Struct { type_name: rust_type, inline_params } => Box::new(
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter().flat_map(|p| p.iter_rust_types())))
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Enum { type_name: rust_type, variant_name: _, inline_params } => Box::new(
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter().flat_map(|p| p.iter_rust_types())))
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
        }
    }
}

impl SerialBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialBody::Fields(fields) => Box::new(
                fields.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
        }
    }
}

impl SerialTupleItem {
    //noinspection DuplicatedCode
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.rust_type.iter()
            .chain(self.value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.value_children.iter_rust_types())
    }
}

impl SerialField {
    //noinspection DuplicatedCode
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.rust_type.iter()
            .chain(self.value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.value_children.iter_rust_types())
    }
}
// endregion