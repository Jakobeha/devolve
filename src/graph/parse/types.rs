use std::collections::HashMap;

use derive_more::Display;
//noinspection RsUnusedImport (intelliJ fails to detect use)
use join_lazy_fmt::Join;

pub struct SerialGraph {
    pub types: HashMap<String, SerialType>,
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
    // TODO: Allow rust type?
    pub rust_type: Option<SerialRustType>,
    pub value: Option<SerialValueHead>,
    pub value_children: SerialBody
}

pub enum SerialType {
    Struct(SerialStructType),
    Enum(SerialEnumType)
}

pub struct SerialStructType {
    pub body: SerialTypeBody,
}

pub struct SerialEnumType {
    pub variants: Vec<SerialEnumVariantType>,
}

pub struct SerialEnumVariantType {
    pub name: String,
    pub body: SerialTypeBody
}

pub enum SerialTypeBody {
    None,
    Tuple(Vec<SerialRustType>),
    Fields(Vec<SerialFieldType>)
}

pub struct SerialFieldType {
    pub name: String,
    pub rust_type: Option<SerialRustType>,
    pub default_value: Option<SerialValueHead>,
    pub default_value_children: SerialBody
}

#[derive(Clone, Display)]
pub enum SerialRustType {
    #[display(fmt = "{}{}", name, "if generic_args.is_empty() { std::borrow::Cow::Borrowed(\"\") } else { std::borrow::Cow::Owned(format!(\"<{}>\", \", \".join(generic_args))) }")]
    Ident {
        name: String,
        generic_args: Vec<SerialRustType>
    },
    #[display(fmt = "&{}", _0)]
    Reference(Box<SerialRustType>),
    #[display(fmt = "({})", "\", \".join(_0)")]
    Tuple(Vec<SerialRustType>),
    #[display(fmt = "[{}; {}]", elem, length)]
    Array {
        elem: Box<SerialRustType>,
        length: usize
    },
    #[display(fmt = "[{}]", _0)]
    Slice(Box<SerialRustType>),
}

pub struct SerialValue {
    head: Option<SerialValueHead>,
    body: SerialBody
}

pub enum SerialValueHead {
    Integer(i64),
    Float(f64),
    String(String),
    Ref {
        node_ident: String,
        field_ident: String
    },
    Array(Vec<SerialValueHead>),
    Tuple(Vec<SerialValueHead>)
}

impl Default for SerialBody {
    fn default() -> Self {
        SerialBody::None
    }
}

impl Default for SerialTypeBody {
    fn default() -> Self {
        SerialTypeBody::None
    }
}
