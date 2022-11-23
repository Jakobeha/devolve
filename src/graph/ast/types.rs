use std::collections::HashMap;
use std::iter::{empty, once};

pub use super::display::*;

use structural_reflection::{RustTypeName, TypeStructureBodyForm};

/// Graph = source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstGraph {
    /// Rust type definitions
    pub type_defs: HashMap<String, AstTypeDef>,
    /// Nodes ("node definitions")
    pub nodes: HashMap<String, AstNode>
}

/// Node = input, output, or effectful computation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    /// Node type (not to be distinguished with rust type) name
    pub node_type: Option<String>,
    pub input_fields: Vec<AstFieldElem>,
    pub output_fields: Vec<AstFieldElem>
}

/// Field elem = field (semantic) or header (aesthetic)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstFieldElem {
    Header { header: AstFieldHeader },
    Field { field: AstField }
}

/// Aesthetic for the UI
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstFieldHeader {
    /// Message. Headers roughly group fields
    Message(String),
    /// Sets an attribute on the entire node.
    ///
    /// In text, this is a header with a specialized format starting with `@`.
    ///
    /// Each node should have at most one of each type of these,
    /// but as this is a syntax AST it isn't enforced.
    NodeAttr(AstNodeAttr)
}

/// Node attribute
///
/// Each node should have at most one of each type of these,
/// but as this is a syntax AST it isn't enforced.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstNodeAttr {
    /// Sets the position of the entire node
    Pos(NodePos),
    /// Sets the primary color of the entire node.
    ///
    /// Primary color is used to group nodes independent of location and type
    /// The secondary color (which may actually be more prominent) is determined by the node type
    /// (groups nodes by node type). It's not represented in the AST since the type is not either.
    PrimaryColor(NodeColor)
}

/// Node position = int vector2
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodePos {
    pub x: i32,
    pub y: i32
}

/// Node color = [OkLCH (OkLab version of LCH)](https://bottosson.github.io/posts/oklab/).
///
/// Both primary (node specific) and secondary (node type specific) color are of this type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeColor {
    pub l: u8,
    pub c: u8,
    pub h: u8
}

/// Node field = input or output
///
/// If this is an output field and `value_head` or `value_children` are defined,
/// then they are the default output for if the computation returns a null output for the field
/// (if it does return something then it overrides the default output)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstField {
    /// Field name (aesthetic, semantically this is an index)
    pub name: String,
    /// Explicit field type, may be unspecified.
    ///
    /// Type may also be inferred from `value_head` and type info may be in `value_children`
    pub rust_type: Option<AstRustType>,
    /// Field type nullability. Note that a default value is considered non-null.
    ///
    /// If input, this means "node accepts if this input is null".
    /// If output, this means "the output may be null and other nodes must accept that"
    pub rust_type_may_be_null: bool,
    /// If this is an output node than this is the default value
    /// if the computation does not return an output.
    pub value_head: Option<AstValueHead>,
    pub value_children: AstValueBody
}

/// Item in a tuple or array field. Same rules as [AstField] apply for the type and value
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTupleItem {
    pub rust_type: Option<AstRustType>,
    pub rust_type_may_be_null: bool,
    pub value: Option<AstValueHead>,
    pub value_children: AstValueBody
}

/// Rust type definition.
///
/// Remember that all Rust types in a DUI graph must be [Copy]-able since there are no linear types
/// and their raw data gets passed around (they don't need to support arbitrary and explicitly defined
/// bits like [Pod](https://docs.rs/bytemuck/latest/bytemuck/trait.Pod.html).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstTypeDef {
    Struct(AstStructTypeDef),
    Enum(AstEnumTypeDef)
}

/// Struct type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstStructTypeDef {
    pub body: AstTypeDefBody,
}

/// Enum type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstEnumTypeDef {
    pub variants: Vec<AstEnumVariantTypeDef>,
}

/// Enum variant in an enum type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstEnumVariantTypeDef {
    pub name: String,
    pub body: AstTypeDefBody
}

/// Type def body in a struct or enum variant type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstTypeDefBody {
    /// Unit struct or variant
    None,
    /// Tuple struct or variant
    Tuple(Vec<AstRustType>),
    /// Struct or variant with named fields
    Fields(Vec<AstFieldTypeDef>)
}

/// Named field in a struct or enum variant type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstFieldTypeDef {
    /// Field name (aesthetic, semantically this is an index)
    pub name: String,
    /// Field type. If null, this must be inferred from the default value
    pub rust_type: Option<AstRustType>,
    /// Struct and variant fields can be nullable! But only limited support,
    /// since they cannot be null in Rust.
    ///
    /// This works because node fields and their struct fields are flattened.
    /// If a node-field is non-null but its type is a struct or enum which supports null fields,
    /// then the sub-field is still considered nullable.
    ///
    /// Default value counting as non-null and other rules of node-fields apply.
    pub rust_type_may_be_null: bool,
    /// Similarly, struct and variant fields can have default values
    pub default_value_head: Option<AstValueHead>,
    pub default_value_children: AstValueBody
}

/// Rust type in AST = rust type name
pub type AstRustType = RustTypeName;

/// Inline value or struct/enum name. In node field or type default value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstValueHead {
    /// Literal = number, string, etc.
    Literal(AstLiteral),
    /// Reference to another node field
    Ref {
        node_name: String,
        field_name: String
    },
    /// Inline array of values (values of are same type, type permits arbitrary length).
    ///
    /// Arrays can alternatively be created by no value head and a tuple [AstValueBody]
    InlineArray(Vec<AstValueHead>),
    /// Inline tuple of values (may have different types, type has fixed types and length)
    ///
    /// Tuples can alternatively be created by no value head and a tuple [AstValueBody]
    InlineTuple(Vec<AstValueHead>),
    /// Structure value
    Struct {
        /// Structure name
        type_name: AstRustType,
        /// Inline tuple items or fields (names are implicit, order must be same as definition).
        ///
        /// Tuple struct items or field struct fields can alternatively be declared by a tuple or
        /// field [AstValueBody], respectively. Unit struct doesn't need inline or body params
        inline_params: Option<Vec<AstValueHead>>,
    },
    /// Enum value
    Enum {
        /// Enum name
        type_name: AstRustType,
        /// Variant
        variant_name: String,
        /// Inline tuple items or fields (names are implicit, order must be same as definition)

        /// Tuple variant items or field variant fields can alternatively be declared by a tuple or
        /// field [AstValueBody], respectively. Unit variant doesn't need inline or body params
        inline_params: Option<Vec<AstValueHead>>,
    },
}

/// Literal = number, string, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum AstLiteral {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

/// Multiline value body.
///
/// Note that this type is intrinsically optional: `Option<AstValueBody>` is always redundant
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstValueBody {
    None,
    /// Tuple, array, or tuple struct or enum variant
    Tuple(Vec<AstTupleItem>),
    /// Field struct or enum variant
    Fields(Vec<AstField>)
}

impl Eq for AstLiteral {}

impl AstGraph {
    /// Create empty graph
    pub fn new() -> AstGraph {
        AstGraph {
            type_defs: HashMap::new(),
            nodes: HashMap::new()
        }
    }
}

impl AstNode {
    /// Create empty node of the named type, or `None` for an untyped node (e.g. input or output)
    pub fn new(node_type: Option<String>) -> AstNode {
        AstNode {
            node_type,
            input_fields: Vec::new(),
            output_fields: Vec::new()
        }
    }
}

impl AstValueBody {
    /// Whether this is for empty/unit, array/tuple/tuple struct, or fields
    pub fn form(&self) -> TypeStructureBodyForm {
        match self {
            AstValueBody::None => TypeStructureBodyForm::None,
            AstValueBody::Tuple(_) => TypeStructureBodyForm::Tuple,
            AstValueBody::Fields(_) => TypeStructureBodyForm::Fields
        }
    }
}

impl Default for AstValueBody {
    fn default() -> Self {
        AstValueBody::None
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
        self.type_defs.values().flat_map(|t| t.iter_rust_types())
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
            .chain(self.default_value_head.iter().flat_map(|value| value.iter_rust_types()))
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
            AstValueHead::Literal(_) => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::Ref { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::InlineTuple(items) => Box::new(
                items.iter().flat_map(|item| item.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueHead::InlineArray(elems) => Box::new(
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

impl AstValueBody {
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&AstRustType> {
        match self {
            AstValueBody::None => Box::new(empty()) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueBody::Tuple(tuple_items) => Box::new(
                tuple_items.iter().flat_map(|f| f.iter_rust_types())
            ) as Box<dyn Iterator<Item=&AstRustType>>,
            AstValueBody::Fields(fields) => Box::new(
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
            .chain(self.value_head.iter().flat_map(|value| value.iter_rust_types()))
            .chain(self.value_children.iter_rust_types())
    }
}
// endregion