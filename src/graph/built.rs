use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::iter::zip;
use std::mem::{MaybeUninit, transmute};
use std::ptr::copy_nonoverlapping;
use crate::graph::error::{GraphIOCheckError, GraphIOCheckErrors, GraphValidationErrors};
use crate::graph::mutable::{MutableGraph, Node as GraphNode, NodeId, NodeInput as GraphNodeInput, NodeInputWithLayout as GraphNodeInputWithLayout, NodeIOType};
use crate::graph::raw::{RawComputeFn, RawData, RawInputs, RawOutputs};

/// Compound view graph.
///
/// A compound view is a graph of nodes which may be subviews, input/output, or computations.
/// It is loaded from a .dui file.
///
/// This graph is internally validated: as long as any given inputs and outputs are valid,
/// a computation can be run unchecked.
pub struct BuiltGraph {
    input_types: Vec<NodeIOType>,
    output_types: Vec<NodeIOType>,
    compute_dag: Vec<Node>,
    outputs: Vec<NodeInput>,
    const_output_data: RawData
}

struct Node {
    compute: RawComputeFn,
    inputs: Vec<NodeInput>,
    cached_input_data: RawData,
    cached_output_data: RawData
}

enum NodeInput {
    Dep(NodeInputDep),
    Const,
    Array(Vec<NodeInput>),
    // Different-sized elems means we need to know the layout
    // (technically we could workaround storing here and it's redundant, but in practice this is easier)
    Tuple(Vec<NodeInputWithLayout>)
}

pub struct NodeInputWithLayout {
    input: NodeInput,
    size: usize,
    align: usize
}

enum NodeInputDep {
    OtherNodeOutput {
        node_idx: usize,
        field_idx: usize
    },
    GraphInput {
        field_idx: usize
    }
}

struct Edge {
    num_outputs: usize,
    input_backwards_offsets: Vec<usize>,
}

impl BuiltGraph {
    pub unsafe fn try_from_but_assume_sorted_if_there_are_no_cycles(graph: MutableGraph) -> Result<Self, GraphValidationErrors> {
        let errors = graph.validate();
        if errors.is_empty() {
            Ok(BuiltGraph::new_unchecked(graph, true))
        } else {
            Err(errors)
        }
    }

    pub unsafe fn new_unchecked(graph: MutableGraph, assume_deps_are_sorted: bool) -> Self {
        debug_assert!(graph.validate().is_empty());

        let input_types = graph.input_types;
        let output_types = graph.output_types;

        let mut sorted_nodes = graph.nodes.into_iter().map(|(node_id, node)| (NodeId(node_id), node)).collect::<Vec<_>>();
        if !assume_deps_are_sorted {
            GraphNode::sort_by_deps(&mut sorted_nodes);
        }
        let node_indices = sorted_nodes.iter().enumerate().map(|(index, (node_id, _))| (*node_id, index)).collect::<HashMap<_, _>>();
        fn get_input_dep(dep: GraphNodeInputDep, node_indices: &HashMap<NodeId, usize>) -> NodeInputDep {
            match &dep {
                GraphNodeInputDep::OtherNodeOutput { id, idx } => NodeInputDep::OtherNodeOutput {
                    node_idx: node_indices[&id],
                    field_idx: *idx
                },
                GraphNodeInputDep::GraphInput { idx } => NodeInputDep::GraphInput {
                    field_idx: *idx
                }
            }
        }
        unsafe fn get_input(input: GraphNodeInput, cached_input_data: &mut Box<[MaybeUninit<u8>]>, node_indices: &HashMap<NodeId, usize>) -> NodeInput {
            match input {
                GraphNodeInput::Hole => panic!("Hole input not valid"),
                GraphNodeInput::Dep(dep) => NodeInput::Dep(get_input_dep(dep, node_indices)),
                GraphNodeInput::Const(const_) => {
                    *cached_input_data = transmute::<Box<[u8]>, Box<[MaybeUninit<u8>]>>(const_);
                    NodeInput::Const
                },
                GraphNodeInput::Array(inputs) => NodeInput::Array(inputs.into_iter().map(|input| get_input(input, cached_input_data, node_indices)).collect()),
                GraphNodeInput::Tuple(inputs_with_layouts) => NodeInput::Tuple(inputs_with_layouts.into_iter().map(|GraphNodeInputWithLayout { input, size, align }| NodeInputWithLayout { input: get_input(input, cached_input_data, node_indices), size, align }).collect())
            }
        }
        unsafe fn get_inputs(input_types: &[NodeIOType], inputs: Vec<GraphNodeInput>, node_indices: &HashMap<NodeId, usize>) -> (RawData, Vec<NodeInput>) {
            let mut cached_input_data = RawData {
                types: input_types.iter().map(|input| *input.rust_type.id).collect::<Vec<_>>(),
                data: input_types.iter().map(|input| Box::new_uninit_slice(*input.rust_type.size)).collect::<Vec<_>>(),
            };
            let inputs = zip(inputs.into_iter(), cached_input_data.data.iter_mut())
                .map(|(input, cached_input_data)| get_input(input, cached_input_data, node_indices))
                .collect::<Vec<_>>();
            (cached_input_data, inputs)
        }
        let compute_dag = sorted_nodes.into_iter().map(|(node_id, node)| {
            let node_type = &graph[&node.node_type];

            let compute = node.compute;
            let cached_output_data = RawData {
                types: node_type.outputs.iter().map(|input| *input.rust_type.id).collect::<Vec<_>>(),
                data: node_type.outputs.iter().map(|input| Box::new_uninit_slice(*input.rust_type.size)).collect::<Vec<_>>(),
            };
            let (cached_input_data, inputs) = get_inputs(&node_type.inputs, node.inputs, &node_indices);

            Node {
                compute,
                inputs,
                cached_input_data,
                cached_output_data
            }
        }).collect::<Vec<_>>();
        let (cached_output_data, outputs) = get_inputs(&output_types, graph.outputs, &node_indices);

        BuiltGraph {
            input_types,
            output_types,
            compute_dag,
            outputs,
            const_output_data: cached_output_data
        }
    }

    pub fn check(&self, input_types: &[TypeId], output_types: &[TypeId]) -> GraphIOCheckErrors {
        let mut errors = Vec::new();

        if input_types.len() != self.input_types.len() {
            errors.push(GraphIOCheckError::InputsCountMismatch {
                actual: input_types.len(),
                expected: self.input_types.len()
            });
        }
        if output_types.len() != self.output_types.len() {
            errors.push(GraphIOCheckError::OutputsCountMismatch {
                actual: output_types.len(),
                expected: self.output_types.len()
            });
        }
        for (actual_type, expected_type) in zip(input_types.iter(), self.input_types.iter()) {
            if actual_type.type_id() != expected_type.rust_type.id {
                errors.push(GraphIOCheckError::InputTypeMismatch {
                    field_name: expected_type.name.clone(),
                    expected: expected_type.rust_type.clone()
                });
            }
        }
        for (actual_type, expected_type) in zip(output_types.iter(), self.output_types.iter()) {
            if actual_type != &expected_type.rust_type.id {
                errors.push(GraphIOCheckError::OutputTypeMismatch {
                    field_name: expected_type.name.clone(),
                    expected: expected_type.rust_type.clone()
                });
            }
        }

        errors
    }

    pub fn compute(&mut self, inputs: RawInputs<'_>, outputs: RawOutputs<'_>) -> Result<(), GraphIOCheckErrors> {
        let errors = self.check(inputs.types, outputs.types);
        if errors.is_empty() {
            Ok(unsafe { self.compute_unchecked(inputs, outputs) })
        } else {
            Err(errors)
        }
    }

    pub unsafe fn compute_unchecked(&mut self, inputs: RawInputs, mut outputs: RawOutputs<'_>) {
        debug_assert!(inputs.len() == self.input_types.len() && outputs.len() == self.output_types.len());

        unsafe fn handle_inputs(inputs: &[NodeInput], input_data: &mut [Box<[MaybeUninit<u8>]>], compute_dag: &Vec<Node>) {
            unsafe fn handle_input(input: &NodeInput, input_data: &mut [MaybeUninit<u8>], compute_dag: &Vec<Node>) {
                match input {
                    NodeInput::Const => {},
                    NodeInput::Dep(dep) => {
                        let other_output = match dep {
                            NodeInputDep::OtherNodeOutput { node_idx, field_idx } => {
                                let other_node = &compute_dag[node_idx];
                                &other_node.cached_output_data.data[field_idx];
                            }
                            NodeInputDep::GraphInput { field_idx } => &inputs.data[field_idx]
                        };
                        debug_assert!(other_output.len() == input_data.len());
                        copy_nonoverlapping(other_output.as_ptr(), input_data as *mut _, other_output.len());
                    }
                    NodeInput::Array(inputs) => {
                        let input_datas = input_data.chunks_mut(inputs.len());
                        for (input, input_data) in zip(inputs, input_datas) {
                            handle_input(input, input_data, compute_dag);
                        }
                    }
                    NodeInput::Tuple(inputs) => {
                        let mut offset = 0;
                        for NodeInputWithLayout { input, size, align } in inputs {
                            if offset % align != 0 {
                                offset += align - offset % align;
                            }
                            let input_data = &mut input_data[offset..offset + size];
                            handle_input(input, input_data, compute_dag);
                            offset += size;
                        }
                    }
                }
            };

            for (input, mut input_data) in zip(inputs, input_data) {
                handle_input(&input, input_data.as_mut(), compute_dag);
            }
        };

        for node in self.compute_dag.iter_mut() {
            // Copy input data to nodes (const data is already there), then compute to output data
            handle_inputs(&node.inputs, &mut node.cached_input_data.data, &self.compute_dag);
            node.compute.run(ctx, RawInputs::from(&node.cached_input_data), RawOutputs::from(&mut node.cached_output_data))
        }

        // Copy const data, then write the rest
        let output_data = outputs.data();
        // const data
        debug_assert!(self.const_output_data.data.len() == output_data.len());
        for (const_data, output_data) in zip(self.const_output_data.data.iter(), output_data.iter_mut()) {
            debug_assert!(const_data.len() == output_data.len());
            copy_nonoverlapping(const_data.as_ptr(), output_data.as_mut_ptr(), const_data.len());
        }
        // the rest
        handle_inputs(&self.outputs, output_data, &self.compute_dag);
    }
}

impl TryFrom<MutableGraph> for BuiltGraph {
    type Error = GraphValidationErrors;

    fn try_from(value: MutableGraph) -> Result<Self, Self::Error> {
        let errors = value.validate();
        if errors.is_empty() {
            Ok(unsafe { BuiltGraph::new_unchecked(value, false) })
        } else {
            Err(errors)
        }
    }
}
