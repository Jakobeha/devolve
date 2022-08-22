use std::collections::{HashMap, HashSet};
use std::iter::empty;
use std::ops::{Index, IndexMut};

use slab::Slab;

pub use node::*;

use crate::graph::error::{GraphFormErrors, GraphValidationError, GraphValidationErrors, NodeCycle};
use crate::graph::mutable::build::GraphBuilder;
use crate::graph::mutable::serialize::GraphSerializer;
use crate::graph::parse::types::SerialGraph;
use crate::misc::try_index::{NotFound, TryIndex, TryIndexMut};
use crate::rust_type::TypeStructure;

mod node;
mod build;
mod serialize;

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

impl MutableGraph {
    pub fn insert_node(&mut self, node: Node) -> NodeId {
        NodeId(self.nodes.insert(node))
    }

    pub fn delete_node(&mut self, id: NodeId) {
        self.nodes.remove(id.0);
    }

    pub fn validate(&self) -> GraphValidationErrors {
        let mut errors = Vec::new();

        if let Err(cycle) = self.check_cycle() {
            errors.push(GraphValidationError::Cycle(cycle))
        }

        todo!("check that input and output types match");
        todo!("check that there are no partially-filled inputs in required regions or output");

        errors
    }

    pub fn iter_node_ids(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.nodes.iter().map(|(id, _)| NodeId(id))
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item=(NodeId, &Node)> {
        self.nodes.iter().map(|(id, node)| (NodeId(id), node))
    }

    pub fn iter_mut_nodes(&mut self) -> impl Iterator<Item=(NodeId, &mut Node)> {
        self.nodes.iter_mut().map(|(id, node)| (NodeId(id), node))
    }

    fn check_cycle(&self) -> Result<(), NodeCycle> {
        let mut not_in_cycle = HashSet::<NodeId>::new();
        for node_id in self.iter_node_ids() {
            // Depth-first search
            let mut visited = HashSet::<NodeId>::new();
            for elem in &not_in_cycle {
                visited.insert(*elem);
            }

            let mut current_chain = Vec::new();
            let mut recurse_stack = Vec::new();
            current_chain.push(node_id);
            recurse_stack.push(0);
            while let Some(next_id) = current_chain.pop() {
                let next_node = &self[next_id];
                let recurse_idx = recurse_stack.pop().unwrap();

                let mut remaining_deps = next_node.iter_dep_nodes().skip(recurse_idx);
                if let Some(next_dep) = remaining_deps.next() {
                    if next_dep == node_id {
                        current_chain.push(node_id);
                        return Err(NodeCycle(current_chain));
                    } else if !visited.contains(&next_dep) {
                        current_chain.push(next_id);
                        recurse_stack.push(recurse_idx + 1);
                    }
                } else {
                    visited.insert(next_id);
                }
            }

            not_in_cycle.insert(node_id);
        }

        Ok(())
    }
}

// region serialization / deserialization
impl TryFrom<SerialGraph> for MutableGraph {
    type Error = GraphFormErrors;

    fn try_from(value: SerialGraph) -> Result<Self, Self::Error> {
        let mut errors = Vec::new();
        // TODO: Module qualifiers?
        let graph = GraphBuilder::build(value, Vec::new(), &mut errors);

        if errors.is_empty() {
            Ok(graph)
        } else {
            Err(errors)
        }
    }
}

impl Into<SerialGraph> for MutableGraph {
    fn into(self) -> SerialGraph {
        GraphSerializer::serialize(self, empty())
    }
}

impl<'a> Into<SerialGraph> for (MutableGraph, &'a [(String, TypeStructure)]) {
    fn into(self) -> SerialGraph {
        GraphSerializer::serialize(self.0, self.1.into_iter())
    }
}
// endregion

// region index boilerplate
impl TryIndex<NodeId> for MutableGraph {
    type Output = Node;

    fn try_index(&self, index: NodeId) -> Result<&Self::Output, NotFound<NodeId>> {
        self.nodes.get(index.0).ok_or(NotFound { index })
    }
}

impl TryIndexMut<NodeId> for MutableGraph {
    fn try_index_mut(&mut self, index: NodeId) -> Result<&mut Self::Output, NotFound<NodeId>> {
        self.nodes.get_mut(index.0).ok_or(NotFound { index })
    }
}

impl Index<NodeId> for MutableGraph {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        self.nodes.index(index.0)
    }
}

impl IndexMut<NodeId> for MutableGraph {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.nodes.index_mut(index.0)
    }
}

impl<'a> TryIndex<&'a NodeTypeName> for MutableGraph {
    type Output = NodeTypeData;

    fn try_index(&self, index: &'a NodeTypeName) -> Result<&Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index(index)
    }
}

impl<'a> TryIndexMut<&'a NodeTypeName> for MutableGraph {
    fn try_index_mut(&mut self, index: &'a NodeTypeName) -> Result<&mut Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index_mut(index)
    }
}

impl<'a> Index<&'a NodeTypeName> for MutableGraph {
    type Output = NodeTypeData;

    fn index(&self, index: &'a NodeTypeName) -> &Self::Output {
        self.types.index(index)
    }
}

impl<'a> IndexMut<&'a NodeTypeName> for MutableGraph {
    fn index_mut(&mut self, index: &'a NodeTypeName) -> &mut Self::Output {
        self.types.get_mut(index).expect("index_mut: key not found")
    }
}
// endregion
