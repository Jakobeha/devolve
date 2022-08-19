use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use join_lazy_fmt::Join;

use snailquote::escape;
use crate::graph::parse::topological_sort::SortByDeps;
use crate::misc::fmt_with_ctx::{DisplayWithCtx, Indent};

pub struct SerialGraph {
    pub rust_types: HashMap<String, SerialType>,
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

#[derive(Clone)]
pub enum SerialRustType {
    Ident {
        name: String,
        generic_args: Vec<SerialRustType>
    },
    Reference(Box<SerialRustType>),
    Tuple(Vec<SerialRustType>),
    Array {
        elem: Box<SerialRustType>,
        length: usize
    },
    Slice(Box<SerialRustType>),
}

pub enum SerialValueHead {
    Integer(i64),
    Float(f64),
    String(String),
    Ref {
        node_name: String,
        field_name: String
    },
    Array(Vec<SerialValueHead>),
    Tuple(Vec<SerialValueHead>)
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

// region Display impls
impl Display for SerialGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut rust_types = self.rust_types.iter().collect::<Vec<_>>();
        rust_types.sort_by_deps();
        let mut nodes = self.nodes.iter().collect::<Vec<_>>();
        nodes.sort_by_deps();

        for (type_name, rust_type) in rust_types {
            writeln!(f, "{}", rust_type.with_ctx(type_name))?;
        }

        for (node_name, node) in nodes {
            writeln!(f, "{}", node.with_ctx(node_name))?;
        }

        Ok(())
    }
}

impl DisplayWithCtx for SerialType {
    type Ctx = String;

    fn fmt(&self, f: &mut Formatter<'_>, type_name: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialType::Struct(struct_type) => {
                writeln!(f, "struct {} {{", type_name)?;
                writeln!(f, "{}", struct_type.body.with_ctx(&Indent(1)))?;
                writeln!(f, "}}")
            }
            SerialType::Enum(enum_type) => {
                writeln!(f, "pub enum {} {{", type_name)?;
                for variant in &enum_type.variants {
                    writeln!(f, "  {}", variant.name)?;
                    writeln!(f, "{}", variant.body.with_ctx(&Indent(2)))?;
                }
                writeln!(f, "}}")
            }
        }
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialTypeBody {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialTypeBody::None => {},
            SerialTypeBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item)?;
                }
            }
            SerialTypeBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field)?;
                }
            }
        }
        Ok(())
    }
}

impl Display for SerialFieldType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type)?;
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialNode {
    type Ctx = String;

    fn fmt(&self, f: &mut Formatter<'_>, node_name: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}", node_name)?;
        if let Some(node_type) = self.node_type.as_ref() {
            write!(f, ": {}", node_type)?;
        }
        writeln!(f, "")?;

        for input_field in &self.input_fields {
            writeln!(f, "  {}", input_field.with_ctx(&Indent(1)))?;
        }
        writeln!(f, "  ===")?;
        for output_field in &self.output_fields {
            writeln!(f, "  {}", output_field.with_ctx(&Indent(1)))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialFieldElem {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialFieldElem::Header { header } => write!(f, "-- {}", header),
            SerialFieldElem::Field(field) => write!(f, "{}", field.with_ctx(indent))
        }
    }
}

impl DisplayWithCtx for SerialField {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type)?;
        }
        if let Some(value) = self.value.as_ref() {
            write!(f, " = {}", value)?;
        }
        if !matches!(self.value_children, SerialBody::None) {
            writeln!(f, "")?;
        }
        write!(f, "{}", self.value_children.with_ctx(&indent.next()))
    }
}

impl DisplayWithCtx for SerialBody {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialBody::None => {},
            SerialBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item.with_ctx(&indent.next()))?;
                }
            }
            SerialBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field.with_ctx(&indent.next()))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialTupleItem {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        match self.value.as_ref() {
            None => write!(f, "_")?,
            Some(value) => write!(f, "{}", value)?
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type)?;
        }
        if !matches!(self.value_children, SerialBody::None) {
            writeln!(f, "")?;
        }
        write!(f, "{}", self.value_children.with_ctx(&indent.next()))
    }
}

impl Display for SerialValueHead {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SerialValueHead::Integer(value) => write!(f, "{}", value),
            SerialValueHead::Float(value) => write!(f, "{}", value),
            SerialValueHead::String(value) => write!(f, "{}", escape(value)),
            SerialValueHead::Ref { node_name: node_ident, field_name: field_ident } => write!(f, "{}.{}", node_ident, field_ident),
            SerialValueHead::Tuple(items) => write!(f, "({})", ", ".join(items)),
            SerialValueHead::Array(elems) => write!(f, "[{}]", ", ".join(elems)),
        }
    }
}

impl Display for SerialRustType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SerialRustType::Ident { name, generic_args } => {
                write!(f, "{}", name)?;
                if !generic_args.is_empty() {
                    write!(f, "<{}>", ", ".join(generic_args))?;
                }
                Ok(())
            }
            SerialRustType::Reference(refd) => write!(f, "&{}", refd),
            SerialRustType::Tuple(items) => write!(f, "({})", ", ".join(items)),
            SerialRustType::Array { elem, length } => write!(f, "[{}; {}]", elem, length),
            SerialRustType::Slice(elem) => write!(f, "[{}]", elem)
        }
    }
}
// endregion