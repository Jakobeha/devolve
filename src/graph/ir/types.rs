use std::collections::HashMap;
use std::mem::{MaybeUninit, size_of};
use derive_more::Display;
use slab::Slab;
use structural_reflection::RustType;
use crate::graph::raw::RawComputeFn;
use crate::ast::types::{AstFieldHeader, AstNodePos};
use crate::raw::NullRegion;

/// Compound view graph.
///
/// A compound view is a graph of nodes which may be subviews, input/output, or computations.
/// It is loaded from a .dui file.
///
/// This graph is well-formed but not validated.
#[derive(Clone)]
pub struct IrGraph {
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
    pub rust_type: RustType,
    pub null_region: NullRegion,
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

#[derive(Clone)]
pub struct Node {
    pub type_name: NodeTypeName,
    pub inputs: Vec<NodeInput>,
    pub default_outputs: Vec<NodeInput>,
    pub compute: Option<RawComputeFn>,
    pub meta: NodeMetadata
}

/// Display info which is not used in actual computations
#[derive(Clone)]
pub struct NodeMetadata {
    pub node_name: String,
    pub pos: Option<AstNodePos>,
    pub input_headers: Vec<FieldHeader>,
    pub output_headers: Vec<FieldHeader>,
}

#[derive(Clone)]
pub struct FieldHeader {
    /// Note that the index counts indices of previous headers,
    /// so unlike usual you *don't* want to add these in reverse.
    pub index: usize,
    pub header: AstFieldHeader
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeTypeName(String);

/// Note that ids in the graph aren't guaranteed ordered, which is why NodeId is not Ord
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(pub usize);

impl NodeInput {
    pub fn const_<T: Copy>(input: T) -> Self {
        let mut bytes = Box::<[u8]>::new_uninit_slice(size_of::<T>());
        let bytes = unsafe {
            bytes.as_mut_ptr().copy_from_nonoverlapping(
                &input as *const T as *const MaybeUninit<u8>,
                size_of::<T>()
            );
            bytes.assume_init()
        };
        NodeInput::Const(bytes)
    }
}

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
