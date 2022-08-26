use std::collections::HashMap;
use derive_more::Display;
use slab::Slab;
use crate::rust_type::RustType;
use crate::graph::raw::RawComputeFn;
use crate::parse::types::{SerialFieldHeader, SerialNodePos};

/// Compound view graph.
///
/// A compound view is a graph of nodes which may be subviews, input/output, or computations.
/// It is loaded from a .dui file.
///
/// This graph is well-formed but not validated.
pub struct MutableGraph {
    pub(in crate::graph) input_types: Vec<NodeIOType>,
    pub(in crate::graph) output_types: Vec<NodeIOType>,
    pub(in crate::graph) types: HashMap<NodeTypeName, NodeTypeData>,
    pub(in crate::graph) nodes: Slab<Node>,
    pub(in crate::graph) outputs: Vec<NodeInput>
}

#[derive(Clone)]
pub struct NodeTypeData {
    pub inputs: Vec<NodeIOType>,
    pub outputs: Vec<NodeIOType>
}

/// Input type or output type
#[derive(Clone)]
pub struct NodeIOType {
    pub name: String,
    pub rust_type: RustType
}

#[derive(Clone)]
pub enum NodeInput {
    Hole,
    Dep(NodeInputDep),
    Const(Box<[u8]>),
    Array(Vec<NodeInput>),
    // Different-sized elems means we need to know the layout
    // (technically we could workaround storing here and it's redundant, but in practice this is easier)
    Tuple(Vec<NodeInputWithLayout>)
}

#[derive(Clone)]
pub struct NodeInputWithLayout {
    pub input: NodeInput,
    pub size: usize,
    pub align: usize
}

#[derive(Clone, Copy)]
pub enum NodeInputDep {
    GraphInput { idx: usize },
    OtherNodeOutput {
        id: NodeId,
        idx: usize
    }
}

pub struct Node {
    pub type_name: NodeTypeName,
    pub inputs: Vec<NodeInput>,
    pub compute: RawComputeFn,
    pub meta: NodeMetadata
}

/// Display info which is not used in actual computations
pub struct NodeMetadata {
    pub node_name: String,
    pub pos: Option<SerialNodePos>,
    pub input_headers: Vec<FieldHeader>,
    pub output_headers: Vec<FieldHeader>,
}

pub struct FieldHeader {
    /// Note that the index counts indices of previous headers,
    /// so unlike usual you *don't* want to add these in reverse.
    pub index: usize,
    pub header: SerialFieldHeader
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeTypeName(String);

/// Note that ids in the graph aren't guaranteed ordered, which is why NodeId is not Ord
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(pub usize);

impl Default for NodeInput {
    fn default() -> Self {
        NodeInput::Hole
    }
}

impl From<String> for NodeTypeName {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl Into<String> for NodeTypeName {
    fn into(self) -> String {
        self.0
    }
}

impl AsRef<str> for NodeTypeName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
