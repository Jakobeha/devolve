use std::collections::HashMap;
use std::mem::{MaybeUninit, size_of};
use slab::Slab;
use structural_reflection::RustType;
use crate::graph::raw::ComputeFn;
use crate::ast::types::{AstFieldHeader, AstNodePos};
use crate::raw::NullRegion;

/// Compound view graph intermediate-representation loaded from a .dui file.
/// The graph is valid to an extent, see below.
///
/// Invariants:
/// - Nodes have types within the [IrGraph]
/// - The # of node inputs and outputs is the same as the # of input and output types,
///   and each input and output corresponds to the type at the same index
/// - All constant inputs are of the right form, all array or tuple inputs are for arrays or compounds
///   with the same length. That is, all inputs have the correct type and nullability *unless* they
///   are dependency inputs, in which case this isn't guaranteed
///
/// Not invariants:
/// - Nodes may have cycles
/// - Nodes are not necessarily sorted in topological order even if there are cycles,
///   *unless* this was directly created from [AstGraph]
/// - Nodes may not have compute
/// - Dependency inputs may have the incorrect type
///
/// If [IrGraph::validate] checks the "not invariants". If it returns no errors, than all of those are held
/// and you can safely unsafely convert to [LowerGraph](crate::lower::LowerGraph).
#[derive(Clone)]
pub struct IrGraph<RuntimeCtx> {
    pub(in crate::graph) input_types: Vec<NodeIOType>,
    pub(in crate::graph) default_inputs: Vec<NodeInput>,
    pub(in crate::graph) output_types: Vec<NodeIOType>,
    pub(in crate::graph) types: HashMap<NodeTypeName, NodeTypeData>,
    pub(in crate::graph) nodes: Slab<Node<RuntimeCtx>>,
    pub(in crate::graph) outputs: Vec<NodeInput>,
    pub(in crate::graph) input_metadata: NodeMetadata,
    pub(in crate::graph) output_metadata: NodeMetadata,
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
pub struct Node<RuntimeCtx> {
    pub type_name: NodeTypeName,
    pub inputs: Vec<NodeInput>,
    pub default_outputs: Vec<NodeInput>,
    pub compute: Option<ComputeFn<RuntimeCtx>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeTypeName(String);

/// Note that ids in the graph aren't guaranteed ordered, which is why NodeId is not Ord
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl NodeMetadata {
    pub fn empty(node_name: String) -> Self {
        Self {
            node_name,
            pos: None,
            input_headers: Vec::new(),
            output_headers: Vec::new(),
        }
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