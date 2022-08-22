use std::collections::{HashMap, HashSet};

use slab::Slab;

use crate::graph::error::{GraphFormError, GraphFormErrors};
use crate::graph::mutable::{MutableGraph, Node, NodeId, NodeTypeData, NodeTypeName};
use crate::graph::parse::topological_sort::SortByDeps;
use crate::graph::parse::types::{SerialBody, SerialGraph, SerialValueHead};
use crate::mutable::ComptimeCtx;
use crate::rust_type::{RustType, TypeStructBodyForm};

mod type_def;
mod node;
mod rust_type;
mod rust_value;

pub(super) struct GraphBuilder<'a> {
    ctx: &'a ComptimeCtx,
    errors: &'a mut GraphFormErrors,
    graph_types: HashSet<String>,
    resolved_rust_types: HashMap<String, RustType>,
    resolved_node_types: HashMap<NodeTypeName, NodeTypeData>,
    resolved_nodes: HashMap<String, (NodeId, Node)>,
}

enum SerialBodyOrInlineTuple {
    SerialBody(SerialBody),
    InlineTuple { items: Vec<SerialValueHead> }
}

impl SerialBodyOrInlineTuple {
    pub fn form(&self) -> TypeStructBodyForm {
        match self {
            SerialBodyOrInlineTuple::SerialBody(body) => body.form(),
            SerialBodyOrInlineTuple::InlineTuple { .. } => TypeStructBodyForm::Tuple
        }
    }
}

impl<'a> GraphBuilder<'a> {
    /// Note: the built graph's nodes are guaranteed to be topologically sorted as long as there are no cycles.
    /// This is not the case for [MutableGraph] in general though.
    pub(super) fn build(graph: SerialGraph, ctx: &'a ComptimeCtx, errors: &'a mut GraphFormErrors) -> MutableGraph {
        GraphBuilder {
            errors,
            ctx,
            graph_types: HashSet::new(),
            resolved_rust_types: HashMap::new(),
            resolved_node_types: HashMap::new(),
            resolved_nodes: HashMap::new(),
        }._build(graph)
    }

    fn _build(mut self, graph: SerialGraph) -> MutableGraph {
        for graph_type in graph.rust_types.keys().cloned() {
            self.graph_types.insert(graph_type);
        }

        let mut sorted_types = graph.rust_types.into_iter().collect::<Vec<_>>();
        sorted_types.sort_by_deps();
        let mut sorted_nodes = graph.nodes.into_iter().collect::<Vec<_>>();
        sorted_nodes.sort_by_deps();

        for (name, type_def) in sorted_types {
            let type_def = self.resolve_type_def(&name, type_def);
            self.resolved_rust_types.insert(name, type_def);
        }
        for (idx, (name, node)) in sorted_nodes.into_iter().enumerate() {
            let node_id = NodeId(idx.wrapping_sub(idx));
            let (node_type, node) = self.resolve_node(&name, node);
            self.resolved_node_types.insert(node.type_name.clone(), node_type);
            self.resolved_nodes.insert(name, (node_id, node));
        }


        // old
        let input_types = match self.resolved_nodes.remove(NodeTypeName::INPUT) {
            None => {
                self.errors.push(GraphFormError::NoInput);
                Vec::new()
            },
            Some((input_id, input_node)) => {
                debug_assert!(input_id == NodeId(usize::MAX), "input id is wrong, should be guaranteed to be first (-1)");
                if !input_node.inputs.is_empty() { // == !input_types.is_empty() as they have the same length
                    self.errors.push(GraphFormError::InputHasInputs);
                }
                if input_node.type_name.as_ref() != NodeTypeName::INPUT {
                    self.errors.push(GraphFormError::InputHasCompute);
                    // Best failure, we have to clone because this may be used somewhere else
                    let input_type_data = self.resolved_node_types.get(&input_node.type_name).unwrap();
                    input_type_data.outputs.clone()
                } else {
                    // Remove input self-type
                    let input_type_data = self.resolved_node_types.remove(&input_node.type_name).unwrap();
                    debug_assert!(input_type_data.inputs.len() == input_node.inputs.len(), "basic sanity check failed");
                    input_type_data.outputs
                }
            }
        };
        let (output_types, outputs) = match self.resolved_nodes.remove(NodeTypeName::OUTPUT) {
            None => {
                self.errors.push(GraphFormError::NoOutput);
                (Vec::new(), Vec::new())
            },
            Some((output_id, output_node)) => {
                debug_assert!(output_id == NodeId(self.resolved_nodes.len()), "output id is wrong, should be guaranteed to be last (resolved_nodes.len())");
                let output_types = if output_node.type_name.as_ref() != NodeTypeName::OUTPUT {
                    self.errors.push(GraphFormError::OutputHasCompute);
                    // Best failure, we have to clone because this may be used somewhere else
                    let output_type_data = self.resolved_node_types.get(&output_node.type_name).unwrap();
                    if !output_type_data.outputs.is_empty() {
                        self.errors.push(GraphFormError::OutputHasOutputs);
                    }
                    output_type_data.inputs.clone()
                } else {
                    // Remove output self-type
                    let output_type_data = self.resolved_node_types.remove(&output_node.type_name).unwrap();
                    debug_assert!(output_type_data.inputs.len() == output_node.inputs.len(), "basic sanity check failed");
                    if !output_type_data.outputs.is_empty() {
                        self.errors.push(GraphFormError::OutputHasOutputs);
                    }
                    output_type_data.inputs
                };
                (output_types, output_node.inputs)
            }
        };

        let mut nodes = Slab::with_capacity(self.resolved_nodes.len());
        // Could remove this allocation but it would require exposing the internals of slab
        let mut sorted_resolved_nodes = self.resolved_nodes.into_values().collect::<Vec<_>>();
        // NodeId is not Ord because ids in the graph aren't guaranteed ordered
        sorted_resolved_nodes.sort_by_key(|(id, _)| id.0);
        for (node_id, node) in sorted_resolved_nodes.into_iter() {
            let node_id2 = nodes.insert(node);
            debug_assert!(node_id.0 == node_id2, "sanity check failed");
        }

        MutableGraph {
            input_types,
            output_types,
            types: self.resolved_node_types,
            nodes,
            outputs
        }
    }
}