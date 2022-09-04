use std::collections::HashMap;
use std::iter::{empty, once};

pub use super::display::*;

use structural_reflection::{RustTypeName, TypeStructureBodyForm};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstGraph {
    pub rust_types: HashMap<String, AstTypeDef>,
    pub nodes: HashMap<String, AstNode>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    pub node_type: Option<String>,
    pub input_fields: Vec<AstFieldElem>,
    pub output_fields: Vec<AstFieldElem>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstFieldElem {
    Header { header: AstFieldHeader },
    Field { field: AstField }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstFieldHeader {
    Message(String),
    Pos(AstNodePos)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstNodePos {
    pub x: i32,
    pub y: i32
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstField {
    pub name: String,
    pub rust_type: Option<AstRustType>,
    pub rust_type_may_be_null: bool,
    pub value: Option<AstValueHead>,
    pub value_children: AstBody
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstBody {
    None,
    Tuple(Vec<AstTupleItem>),
    Fields(Vec<AstField>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTupleItem {
    pub rust_type: Option<AstRustType>,
    pub rust_type_may_be_null: bool,
    pub value: Option<AstValueHead>,
    pub value_children: AstBody
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstTypeDef {
    Struct(AstStructTypeDef),
    Enum(AstEnumTypeDef)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstStructTypeDef {
    pub body: AstTypeDefBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstEnumTypeDef {
    pub variants: Vec<AstEnumVariantTypeDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstEnumVariantTypeDef {
    pub name: String,
    pub body: AstTypeDefBody
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstTypeDefBody {
    None,
    Tuple(Vec<AstRustType>),
    Fields(Vec<AstFieldTypeDef>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstFieldTypeDef {
    pub name: String,
    pub rust_type: Option<AstRustType>,
    pub rust_type_may_be_null: bool,
    pub default_value: Option<AstValueHead>,
    pub default_value_children: AstBody
}

pub type AstRustType = RustTypeName;

#[derive(Debug, Clone, PartialEq)]
pub enum AstValueHead {
    Integer(i64),
    Float(f64),
    String(String),
    Ref {
        node_name: String,
        field_name: String
    },
    Array(Vec<AstValueHead>),
    Tuple(Vec<AstValueHead>),
    Struct {
        type_name: AstRustType,
        inline_params: Option<Vec<AstValueHead>>,
    },
    Enum {
        type_name: AstRustType,
        variant_name: String,
        inline_params: Option<Vec<AstValueHead>>,
    },
}

impl Eq for AstValueHead {}

impl AstGraph {
    pub fn new() -> AstGraph {
        AstGraph {
            rust_types: HashMap::new(),
            nodes: HashMap::new()
        }
    }
}

impl AstNode {
    pub fn new(node_type: Option<String>) -> AstNode {
        AstNode {
            node_type,
            input_fields: Vec::new(),
            output_fields: Vec::new()
        }
    }
}

impl AstBody {
    pub fn form(&self) -> TypeStructureBodyForm {
        match self {
            AstBody::None => TypeStructureBodyForm::None,
            AstBody::Tuple(_) => TypeStructureBodyForm::Tuple,
            AstBody::Fields(_) => TypeStructureBodyForm::Fields
        }
    }
}

impl Default for AstBody {
    fn default() -> Self {
        AstBody::None
    }
}

impl Default for AstTypeDefBody {
    fn default() -> Self {
        AstTypeDefBody::None
    }
}

// region iter rust types
impl AstGraph {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.rust_types.values().flat_map(|t| t.iter_rust_types())
            .chain(self.nodes.values().flat_map(|n| n.iter_rust_types()))
    }
}

impl AstTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstTypeDef::Struct(s) => Box::new(s.iter_rust_types()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstTypeDef::Enum(e) => Box::new(e.iter_rust_types()) as Box<dyn Iterator<Item=&AstRustType>>
        }
    }
}

impl AstStructTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.body.iter_rust_types()
    }
}

impl AstEnumTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.variants.iter().flat_map(|v| v.body.iter_rust_types())
    }
}

impl AstTypeDefBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstTypeDefBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstTypeDefBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter()
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstTypeDefBody::Fields(fields) => Box::new(
                fields.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
        }
    }
}

impl AstFieldTypeDef {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.rust_type.iter()
            .chain(self.default_value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.default_value_children.iter_rust_types())
    }
}

impl AstNode {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.input_fields.iter().flat_map(|f| f.iter_rust_types())
            .chain(self.output_fields.iter().flat_map(|f| f.iter_rust_types()))
    }
}

impl AstFieldElem {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstFieldElem::Field { field } => Box::new(field.iter_rust_types()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstFieldElem::Header { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
        }
    }
}

impl AstValueHead {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstValueHead::Integer(_) => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Float(_) => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::String(_) => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Ref { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Tuple(items) => Box::new(
                items.iter().flat_map(|item| item.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Array(elems) => Box::new(
                elems.iter().flat_map(|elem| elem.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Struct { type_name: rust_type, inline_params } => Box::new(
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter().flat_map(|p| p.iter_rust_types())))
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Enum { type_name: rust_type, variant_name: _, inline_params } => Box::new(
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter().flat_map(|p| p.iter_rust_types())))
            ) as Box<dyn Iterator<Item=&AstRustType>>,
        }
    }
}

impl AstBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstBody::Fields(fields) => Box::new(
                fields.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
        }
    }
}

impl AstTupleItem {
    //noinspection DuplicatedCode
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.rust_type.iter()
            .chain(self.value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.value_children.iter_rust_types())
    }
}

impl AstField {
    //noinspection DuplicatedCode
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        self.rust_type.iter()
            .chain(self.value.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.value_children.iter_rust_types())
    }
}
// endregion