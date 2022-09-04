use std::collections::{HashMap, HashSet};

use slab::Slab;

use crate::graph::error::{GraphFormError, GraphFormErrors};
use crate::graph::ir::{IrGraph, Node, NodeId, NodeTypeData, NodeTypeName};
use crate::graph::ast::topological_sort::SortByDeps;
use crate::graph::ast::types::{AstBody, AstGraph, AstValueHead};
use crate::graph::StaticStrs;
use crate::ir::ComptimeCtx;
use structural_reflection::{RustType, TypeStructureBodyForm};

mod type_def;
mod node;
mod rust_type;
mod rust_value;

pub(super) struct ForwardNode {
    pub(super) input_field_names: Option<Vec<String>>
}

pub(super) struct GraphBuilder<'a> {
    ctx: &'a ComptimeCtx,
    errors: &'a mut GraphFormErrors,
    forward_resolved_rust_types: HashSet<String>,
    forward_resolved_nodes: HashMap<String, (NodeId, ForwardNode)>,
    resolved_rust_types: HashMap<String, RustType>,
    resolved_node_types: HashMap<NodeTypeName, NodeTypeData>,
    resolved_nodes: HashMap<String, (NodeId, Node)>,
}

enum AstBodyOrInlineTuple {
    AstBody(AstBody),
    InlineTuple { items: Vec<AstValueHead> }
}

impl AstBodyOrInlineTuple {
    pub fn form(&self) -> TypeStructureBodyForm {
        match self {
            AstBodyOrInlineTuple::AstBody(body) => body.form(),
            AstBodyOrInlineTuple::InlineTuple { .. } => TypeStructureBodyForm::Tuple
        }
    }
}

impl<'a> GraphBuilder<'a> {
    /// Note: the built graph's nodes are guaranteed to be topologically sorted as long as there are no cycles.
    /// This is not the case for [MutableGraph] in general though.
    pub(super) fn build(graph: AstGraph, ctx: &'a ComptimeCtx, errors: &'a mut GraphFormErrors) -> IrGraph {
        GraphBuilder {
            errors,
            ctx,
            forward_resolved_rust_types: HashSet::new(),
            forward_resolved_nodes: HashMap::new(),
            resolved_rust_types: HashMap::new(),
            resolved_node_types: HashMap::new(),
            resolved_nodes: HashMap::new(),
        }._build(graph)
    }

    fn _build(mut self, graph: AstGraph) -> IrGraph {
        // sort by DAG so we can handle backward references
        let mut sorted_types = graph.rust_types.into_iter().collect::<Vec<_>>();
        sorted_types.sort_by_deps();
        let mut sorted_nodes = graph.nodes.into_iter().collect::<Vec<_>>();
        sorted_nodes.sort_by_deps();

        // Get declared types and nodes so we can handle forward references
        for (name, _) in &sorted_types {
            self.forward_resolved_rust_types.insert(name.clone());
        }
        for (idx, (name, node)) in sorted_nodes.iter().enumerate() {
            let node_id = NodeId(idx.wrapping_sub(1));
            let forward_node = self.forward_resolve_node(node);
            self.forward_resolved_nodes.insert(name.clone(), (node_id, forward_node));
        }

        // Resolve individual types and nodes, using backward and forward references via the above
        for (name, type_def) in sorted_types {
            let type_def = self.resolve_type_def(&name, type_def);
            self.resolved_rust_types.insert(name, type_def);
        }
        for (idx, (name, node)) in sorted_nodes.into_iter().enumerate() {
            let node_id = NodeId(idx.wrapping_sub(1));
            let (node_type, node) = self.resolve_node(&name, node);
            self.resolved_node_types.insert(node.type_name.clone(), node_type);
            self.resolved_nodes.insert(name, (node_id, node));
        }


        // get inputs and outputs
        let input_types = match self.resolved_nodes.remove(StaticStrs::INPUT_NODE) {
            None => {
                self.errors.push(GraphFormError::NoInput);
                Vec::new()
            },
            Some((input_id, input_node)) => {
                debug_assert!(input_id == NodeId(usize::MAX), "input id is wrong, should be guaranteed to be first (-1)");
                if !input_node.inputs.is_empty() { // == !input_types.is_empty() as they have the same length
                    self.errors.push(GraphFormError::InputHasInputs);
                }
                if input_node.type_name.as_ref() != StaticStrs::INPUT_NODE {
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
        let (output_types, outputs) = match self.resolved_nodes.remove(StaticStrs::OUTPUT_NODE) {
            None => {
                self.errors.push(GraphFormError::NoOutput);
                (Vec::new(), Vec::new())
            },
            Some((output_id, output_node)) => {
                debug_assert!(output_id == NodeId(self.resolved_nodes.len()), "output id is wrong, should be guaranteed to be last (resolved_nodes.len())");
                let output_types = if output_node.type_name.as_ref() != StaticStrs::OUTPUT_NODE {
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

        // build graph
        let mut nodes = Slab::with_capacity(self.resolved_nodes.len());
        // Could remove this allocation but it would require exposing the internals of slab
        let mut sorted_resolved_nodes = self.resolved_nodes.into_values().collect::<Vec<_>>();
        // NodeId is not Ord because ids in the graph aren't guaranteed ordered
        sorted_resolved_nodes.sort_by_key(|(id, _)| id.0);
        for (node_id, node) in sorted_resolved_nodes.into_iter() {
            let node_id2 = nodes.insert(node);
            debug_assert!(node_id.0 == node_id2, "sanity check failed");
        }

        IrGraph {
            input_types,
            output_types,
            types: self.resolved_node_types,
            nodes,
            outputs
        }
    }
}