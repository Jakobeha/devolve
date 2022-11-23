use std::collections::HashMap;
use std::mem::{MaybeUninit, size_of};
use slab::Slab;
use structural_reflection::RustType;
use crate::graph::raw::ComputeFn;
use crate::ast::types::{AstFieldHeader, NodeColor, NodePos};
use crate::misc::inline_ptr::InlinePtr;
use crate::raw::{ConstantPool, NullRegion};

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
/// - Nodes are not necessarily sorted in topological order (regardless if there are cycles),
///   *unless* this was directly created from [AstGraph]
/// - Nodes may not have compute
/// - Dependency inputs may have the incorrect type
///
/// If [IrGraph::validate] checks the "not invariants". If it returns no errors, than all of those are held
/// and you can safely unsafely convert to [LowerGraph](crate::lower::LowerGraph).
pub struct IrGraph<RuntimeCtx: 'static + ?Sized> {
    /// Graph input (input "node") types
    pub(in crate::graph) input_types: Vec<NodeIOType>,
    /// Graph input (input "node") default values (all must be constants)
    pub(in crate::graph) default_inputs: Vec<NodeIO>,
    /// Graph output (output "node") types
    pub(in crate::graph) output_types: Vec<NodeIOType>,
    /// Graph output (output "node") values
    pub(in crate::graph) outputs: Vec<NodeIO>,
    /// Rust types defined or imported into the graph */
    pub(in crate::graph) types: HashMap<NodeTypeName, NodeTypeData>,
    /// Nodes = computations
    pub(in crate::graph) nodes: Slab<Node<RuntimeCtx>>,
    /** Allocated constants (currently only strings) so we can have constant references */
    pub(in crate::graph) constant_pool: ConstantPool,
    /** Graph input (input "node") metadata */
    pub(in crate::graph) input_metadata: NodeMetadata,
    /** Graph output (output "node") metadata */
    pub(in crate::graph) output_metadata: NodeMetadata,
}

/// Node type definition
#[derive(Clone)]
pub struct NodeTypeData {
    /// Input field types
    pub inputs: Vec<NodeIOType>,
    /// Output field types
    pub outputs: Vec<NodeIOType>
}

/// Input type or output field type
#[derive(Debug, Clone)]
pub struct NodeIOType {
    pub name: String,
    pub rust_type: RustType,
    pub null_region: NullRegion,
}

/// Node input or default output; or graph output or default input
///
/// Notice the inversion: graph "inputs" are actually output values, and graph "outputs" are
/// actually input values, when we are inside of the graph. They are only actually inputs/outputs
/// as in the name when we are outside.
#[derive(Debug, Clone)]
pub enum NodeIO {
    /// Unset (null)
    Hole,
    /// References a graph input or another node output
    Dep(NodeIODep),
    /// Constant inline value (e.g. bool, number)
    ConstInline(Box<[u8]>),
    /// Constant reference (e.g. string)
    ConstRef(InlinePtr),
    /// Array of values (same type, arbitrary length)
    Array(Vec<NodeIO>),
    /// Tuple of values (may have different types, fixed length)
    ///
    /// Different-sized elems means we need to know the layout.
    /// Technically we could workaround storing here and it's redundant,
    /// but in practice this is easier
    Tuple(Vec<NodeIOWithLayout>)
}

/// Node input or default output with layout information (size and alignment)
#[derive(Debug, Clone)]
pub struct NodeIOWithLayout {
    pub input: NodeIO,
    pub size: usize,
    pub align: usize
}

/// Reference to a graph input or another node output
#[derive(Debug, Clone, Copy)]
pub enum NodeIODep {
    GraphInput { idx: usize },
    OtherNodeOutput {
        id: NodeId,
        idx: usize
    }
}

/// Node = effectful computation / function
pub struct Node<RuntimeCtx: 'static + ?Sized> {
    /// Node type (function name)
    pub type_name: NodeTypeName,
    /// Node inputs
    pub inputs: Vec<NodeIO>,
    /// Node default outputs - filled if the function returns null for some of its outputs.
    pub default_outputs: Vec<NodeIO>,
    /// Actual compute function referenced by `type_name`.
    pub compute: Option<ComputeFn<RuntimeCtx>>,
    /// Node metadata for the UI (aesthetic)
    pub meta: NodeMetadata
}

/// Aesthetic/UI info which is not used in actual computations
#[derive(Clone)]
pub struct NodeMetadata {
    /// Node name (distinct from node type).
    ///
    /// Different nodes *must* have different names (or the conversion to AST will not work).
    pub node_name: String,
    /// Node position in the UI. If unset, will be layout automatically
    pub pos: Option<NodePos>,
    /// Node color in the UI. If unset, will be the default color
    pub color: Option<NodeColor>,
    /// Input headers.
    ///
    /// Note that they are inserted in a weird way (ascending index) when converting back into AST
    /// (see [FieldHeader]`.index`)
    pub input_headers: Vec<FieldHeader>,
    /// Output headers.
    ///
    /// Note that they are inserted in a weird way (ascending index) when converting back into AST.
    /// (see [FieldHeader]`.index`)
    pub output_headers: Vec<FieldHeader>,
}

#[derive(Clone)]
pub struct FieldHeader {
    /// Note that the index counts indices of previous headers,
    /// so unlike usual you *don't* want to add these in reverse.
    pub index: usize,
    pub header: AstFieldHeader
}

/// Node type name = string [newtype](https://wiki.haskell.org/Newtype)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeTypeName(String);

/// Note that ids in the graph aren't guaranteed ordered, which is why NodeId is not Ord
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(pub usize);

impl NodeIO {
    /// Create an inline constant node input from the given value
    /// (this function converts it into [raw](MaybeUninit) data for you, safe because the type is
    /// [Copy]-able)
    pub fn inline_const<T: Copy>(input: T) -> Self {
        let mut bytes = Box::<[u8]>::new_uninit_slice(size_of::<T>());
        let bytes = unsafe {
            bytes.as_mut_ptr().copy_from_nonoverlapping(
                &input as *const T as *const MaybeUninit<u8>,
                size_of::<T>()
            );
            bytes.assume_init()
        };
        NodeIO::ConstInline(bytes)
    }
}

impl Default for NodeIO {
    fn default() -> Self {
        NodeIO::Hole
    }
}

impl NodeMetadata {
    /// Empty node metadata except for the node name
    /// (required, can pass a randomly-generated unique name if you need to)
    pub fn empty(node_name: String) -> Self {
        Self {
            node_name,
            pos: None,
            color: None,
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


impl<RuntimeCtx: 'static + ?Sized> Clone for IrGraph<RuntimeCtx> {
    fn clone(&self) -> Self {
        Self {
            input_types: self.input_types.clone(),
            default_inputs: self.default_inputs.clone(),
            output_types: self.output_types.clone(),
            types: self.types.clone(),
            nodes: self.nodes.clone(),
            outputs: self.outputs.clone(),
            constant_pool: self.constant_pool.clone(),
            input_metadata: self.input_metadata.clone(),
            output_metadata: self.output_metadata.clone(),
        }
    }
}

impl<RuntimeCtx: 'static + ?Sized> Clone for Node<RuntimeCtx> {
    fn clone(&self) -> Self {
        Self {
            type_name: self.type_name.clone(),
            inputs: self.inputs.clone(),
            default_outputs: self.default_outputs.clone(),
            compute: self.compute.clone(),
            meta: self.meta.clone(),
        }
    }
}