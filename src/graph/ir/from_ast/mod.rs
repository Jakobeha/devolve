use std::collections::{HashMap, HashSet};

use slab::Slab;

use crate::graph::error::{GraphFormError, GraphFormErrors};
use crate::graph::ir::{IrGraph, Node, NodeId, NodeTypeData, NodeTypeName};
use crate::graph::ast::topological_sort::SortByDeps;
use crate::graph::ast::types::{AstBody, AstGraph, AstValueHead};
use crate::graph::StaticStrs;
use crate::raw::ConstantPool;
use crate::ir::{ComptimeCtx, NodeMetadata};
use structural_reflection::{RustType, TypeStructureBodyForm};

mod type_def;
mod node;
mod rust_type;
mod rust_value;

pub(super) struct ForwardNode {
    pub(super) input_field_names: Option<Vec<String>>,
}

pub(super) struct GraphBuilder<'a, RuntimeCtx: 'static + ?Sized> {
    ctx: &'a ComptimeCtx<RuntimeCtx>,
    errors: &'a mut GraphFormErrors,
    constant_pool: ConstantPool,
    forward_resolved_type_defs: HashSet<String>,
    forward_resolved_nodes: HashMap<String, (NodeId, ForwardNode)>,
    node_names: Vec<String>,
    resolved_rust_types: HashMap<String, RustType>,
    resolved_node_types: HashMap<NodeTypeName, NodeTypeData>,
    resolved_nodes: HashMap<String, (NodeId, Node<RuntimeCtx>)>,
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

impl<'a, RuntimeCtx> GraphBuilder<'a, RuntimeCtx> {
    /// Note: the built graph's nodes are guaranteed to be topologically sorted as long as there are no cycles.
    /// This is not the case for [IrGraph] in general though.
    pub(super) fn build(graph: AstGraph, ctx: &'a ComptimeCtx<RuntimeCtx>, errors: &'a mut GraphFormErrors) -> IrGraph<RuntimeCtx> {
        GraphBuilder {
            errors,
            ctx,
            constant_pool: ConstantPool::new(),
            forward_resolved_type_defs: HashSet::new(),
            forward_resolved_nodes: HashMap::new(),
            node_names: Vec::new(),
            resolved_rust_types: HashMap::new(),
            resolved_node_types: HashMap::new(),
            resolved_nodes: HashMap::new(),
        }._build(graph)
    }

    fn _build(mut self, graph: AstGraph) -> IrGraph<RuntimeCtx> {
        // sort by DAG so we can handle backward references
        let mut sorted_types = graph.rust_types.into_iter().collect::<Vec<_>>();
        sorted_types.sort_by_deps();
        let mut sorted_nodes = graph.nodes.into_iter().collect::<Vec<_>>();
        sorted_nodes.sort_by_deps();

        // Get declared types and nodes so we can handle forward references
        for (type_def_name, _) in &sorted_types {
            self.forward_resolved_type_defs.insert(type_def_name.clone());
        }
        for (idx, (node_name, node)) in sorted_nodes.iter().enumerate() {
            let node_id = NodeId(idx.wrapping_sub(1));
            let forward_node = self.forward_resolve_node(node);
            self.forward_resolved_nodes.insert(node_name.clone(), (node_id, forward_node));
            self.node_names.push(node_name.clone());
        }

        // Resolve individual types and nodes, using backward and forward references via the above
        for (type_def_name, type_def) in sorted_types {
            let type_def = self.resolve_type_def(&type_def_name, type_def);
            self.resolved_rust_types.insert(type_def_name, type_def);
        }
        for (idx, (node_name, node)) in sorted_nodes.into_iter().enumerate() {
            let node_id = NodeId(idx.wrapping_sub(1));
            let (node_type, node) = self.resolve_node(&node_name, node_id, node);
            self.resolved_node_types.insert(node.type_name.clone(), node_type);
            self.resolved_nodes.insert(node_name, (node_id, node));
        }


        // get inputs and outputs
        let (input_types, default_inputs, input_metadata) = match self.resolved_nodes.remove(StaticStrs::INPUT_NODE) {
            None => {
                self.errors.push(GraphFormError::NoInput);
                (Vec::new(), Vec::new(), NodeMetadata::empty(StaticStrs::INPUT_NODE.to_string()))
            },
            Some((input_id, input_node)) => {
                debug_assert!(input_id == NodeId(usize::MAX), "input id is wrong, should be guaranteed to be first (-1)");
                let input_types = if input_node.type_name.as_ref() != StaticStrs::INPUT_NODE {
                    self.errors.push(GraphFormError::InputHasCompute);
                    // Best failure, we have to clone because this may be used somewhere else
                    let input_type_data = self.resolved_node_types.get(&input_node.type_name).unwrap();
                    if !input_node.inputs.is_empty() { // == !input_types.is_empty() as they have the same length
                        self.errors.push(GraphFormError::InputHasInputs);
                    }
                    input_type_data.outputs.clone()
                } else {
                    // Remove input self-type
                    let input_type_data = self.resolved_node_types.remove(&input_node.type_name).unwrap();
                    debug_assert!(input_type_data.inputs.len() == input_node.inputs.len(), "basic sanity check failed");
                    if !input_node.inputs.is_empty() { // == !input_types.is_empty() as they have the same length
                        self.errors.push(GraphFormError::InputHasInputs);
                    }
                    input_type_data.outputs
                };
                (input_types, input_node.default_outputs, input_node.meta)
            }
        };
        let (output_types, outputs, output_metadata) = match self.resolved_nodes.remove(StaticStrs::OUTPUT_NODE) {
            None => {
                self.errors.push(GraphFormError::NoOutput);
                (Vec::new(), Vec::new(), NodeMetadata::empty(StaticStrs::OUTPUT_NODE.to_string()))
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
                (output_types, output_node.inputs, output_node.meta)
            }
        };

        // Put together and return
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
            default_inputs,
            types: self.resolved_node_types,
            nodes,
            outputs,
            constant_pool: self.constant_pool,
            input_metadata,
            output_metadata
        }
    }
}