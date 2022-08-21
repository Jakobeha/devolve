use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::{empty, once};
use join_lazy_fmt::Join;

use snailquote::escape;
use crate::graph::parse::topological_sort::SortByDeps;
use crate::misc::fmt_with_ctx::{DisplayWithCtx, DisplayWithCtx2, Indent};
use crate::rust_type::{RustTypeName, TypeStructBodyForm, SimpleNamesInScope};

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
    // TODO: Allow rust type here?
    pub rust_type: Option<SerialRustType>,
    pub value: Option<SerialValueHead>,
    pub value_children: SerialBody
}

pub enum SerialTypeDef {
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

impl Default for SerialTypeBody {
    fn default() -> Self {
        SerialTypeBody::None
    }
}

// region Display impls
impl Display for SerialGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let snis = self.iter_rust_types()
            .flat_map(|rust_type| rust_type.iter_snis())
            .map(String::from).collect::<HashSet<_>>();

        let mut rust_types = self.rust_types.iter().collect::<Vec<_>>();
        rust_types.sort_by_deps();
        let mut nodes = self.nodes.iter().collect::<Vec<_>>();
        nodes.sort_by_deps();

        for (type_name, rust_type) in rust_types {
            writeln!(f, "{}", rust_type.with_ctx((&snis, type_name)))?;
        }

        for (node_name, node) in nodes {
            writeln!(f, "{}", node.with_ctx((&snis, node_name)))?;
        }

        Ok(())
    }
}

impl DisplayWithCtx2 for SerialTypeDef {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, type_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialTypeDef::Struct(struct_type) => {
                writeln!(f, "struct {} {{", type_name)?;
                writeln!(f, "{}", struct_type.body.with_ctx((snis, &Indent(1))))?;
                writeln!(f, "}}")
            }
            SerialTypeDef::Enum(enum_type) => {
                writeln!(f, "pub enum {} {{", type_name)?;
                for variant in &enum_type.variants {
                    writeln!(f, "  {}", variant.name)?;
                    writeln!(f, "{}", variant.body.with_ctx((snis, &Indent(2))))?;
                }
                writeln!(f, "}}")
            }
        }
    }
}

impl DisplayWithCtx2 for SerialTypeBody {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialTypeBody::None => {},
            SerialTypeBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item.display(snis))?;
                }
            }
            SerialTypeBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field.with_ctx(snis))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialFieldType {
    type Ctx = SimpleNamesInScope;

    fn fmt(&self, f: &mut Formatter<'_>, snis: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialNode {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, node_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", node_name)?;
        if let Some(node_type) = self.node_type.as_ref() {
            write!(f, ": {}", node_type)?;
        }
        writeln!(f, "")?;

        for input_field in &self.input_fields {
            writeln!(f, "  {}", input_field.with_ctx((snis, &Indent(1))))?;
        }
        writeln!(f, "  ===")?;
        for output_field in &self.output_fields {
            writeln!(f, "  {}", output_field.with_ctx((snis, &Indent(1))))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialFieldElem {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialFieldElem::Header { header } => write!(f, "-- {}", header),
            SerialFieldElem::Field(field) => write!(f, "{}", field.with_ctx((snis, indent)))
        }
    }
}

impl DisplayWithCtx2 for SerialField {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        if let Some(value) = self.value.as_ref() {
            write!(f, " = {}", value.with_ctx(snis))?;
        }
        if !matches!(self.value_children, SerialBody::None) {
            writeln!(f, "")?;
        }
        write!(f, "{}", self.value_children.with_ctx((snis, &indent.next())))
    }
}

impl DisplayWithCtx2 for SerialBody {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialBody::None => {},
            SerialBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item.with_ctx((snis, &indent.next())))?;
                }
            }
            SerialBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field.with_ctx((snis, &indent.next())))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialTupleItem {
    type Ctx1 = SimpleNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self.value.as_ref() {
            None => write!(f, "_")?,
            Some(value) => write!(f, "{}", value.with_ctx(snis))?
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        if !matches!(self.value_children, SerialBody::None) {
            writeln!(f, "")?;
        }
        write!(f, "{}", self.value_children.with_ctx((snis, &indent.next())))
    }
}

impl DisplayWithCtx for SerialValueHead {
    type Ctx = SimpleNamesInScope;

    //noinspection DuplicatedCode
    fn fmt(&self, f: &mut Formatter<'_>, snis: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialValueHead::Integer(value) => write!(f, "{}", value),
            SerialValueHead::Float(value) => write!(f, "{}", value),
            SerialValueHead::String(value) => write!(f, "{}", escape(value)),
            SerialValueHead::Ref { node_name: node_ident, field_name: field_ident } => write!(f, "{}.{}", node_ident, field_ident),
            SerialValueHead::Tuple(items) => write!(f, "({})", ", ".join(items.iter().map(|x| x.with_ctx(snis)))),
            SerialValueHead::Array(elems) => write!(f, "[{}]", ", ".join(elems.iter().map(|x| x.with_ctx(snis)))),
            SerialValueHead::Struct { type_name: rust_type, inline_params } => {
                write!(f, "{}", rust_type.display(snis))?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(snis))))?;
                }
                Ok(())
            },
            SerialValueHead::Enum { type_name: rust_type, variant_name, inline_params } => {
                write!(f, "{}::{}", rust_type.display(snis), variant_name)?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(snis))))?;
                }
                Ok(())
            },
        }
    }
}
// endregion

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

impl SerialStructType {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.body.iter_rust_types()
    }
}

impl SerialEnumType {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        self.variants.iter().flat_map(|v| v.body.iter_rust_types())
    }
}

impl SerialTypeBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&SerialRustType> {
        match self {
            SerialTypeBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialTypeBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter()
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialTypeBody::Fields(fields) => Box::new(
                fields.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
        }
    }
}

impl SerialFieldType {
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
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter_rust_types()))
            ) as Box<dyn Iterator<Item=&SerialRustType>>,
            SerialValueHead::Enum { type_name: rust_type, variant_name: _, inline_params } => Box::new(
                once(rust_type).chain(inline_params.iter().flat_map(|p| p.iter_rust_types()))
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