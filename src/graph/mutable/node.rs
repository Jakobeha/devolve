use std::cmp::Ordering;
use derive_more::Display;
use crate::rust_type::RustType;
use crate::graph::raw::RawComputeFn;

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
    pub input_headers: Vec<FieldHeader>,
    pub output_headers: Vec<FieldHeader>,
}

pub struct FieldHeader {
    /// Note that the index counts indices of previous headers,
    /// so unlike usual you *don't* want to add these in reverse.
    pub index: usize,
    pub header: String
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeTypeName(String);

/// Note that ids in the graph aren't guaranteed ordered, which is why NodeId is not Ord
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(pub usize);


impl Node {
    pub fn depends_on(&self, other_id: NodeId) -> bool {
        self.iter_dep_nodes().any(|dep| dep == other_id)
    }

    pub fn iter_deps(&self) -> impl Iterator<Item=NodeInputDep> + '_ {
        self.inputs.iter().flat_map(|input| input.deps())
    }

    pub fn iter_dep_nodes(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.inputs.iter().flat_map(|input| input.dep_nodes())
    }

    pub fn sort_by_deps(nodes: &mut [(NodeId, Self)]) {
        nodes.sort_by(|(a_id, a), (b_id, b)| {
            match (a.depends_on(*b_id), b.depends_on(*a_id)) {
                (true, true) => unreachable!("should've been detected in cycle"),
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => Ordering::Equal
            }
        })
    }
}

impl NodeInput {
    pub fn deps(&self) -> impl Iterator<Item=NodeInputDep> + '_ {
        match &self {
            NodeInput::Hole | NodeInput::Const(_) => Box::new(std::iter::empty()) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Dep(dep) => Box::new(std::iter::once(*dep)) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Array(inputs) => Box::new(inputs.iter().flat_map(|input| input.deps())) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Tuple(inputs_with_layouts) => Box::new(inputs_with_layouts.iter().flat_map(|input_with_layout| input_with_layout.input.deps())) as Box<dyn Iterator<Item=NodeInputDep>>
        }
    }

    pub fn dep_nodes(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.deps().filter_map(|dep| match dep {
            NodeInputDep::GraphInput { idx: _ } => None,
            NodeInputDep::OtherNodeOutput { id, idx: _ } => Some(id)
        })
    }
}

impl Default for NodeInput {
    fn default() -> Self {
        NodeInput::Hole
    }
}

impl NodeTypeName {
    pub const INPUT: &'static str = "Input";
    pub const OUTPUT: &'static str = "Output";
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
