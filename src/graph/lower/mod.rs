use std::collections::HashMap;
use std::iter::{repeat, zip};
use std::mem::{MaybeUninit, size_of};
use std::ptr::{copy_nonoverlapping, null};
use crate::graph::error::{GraphIOCheckError, GraphIOCheckErrors, GraphValidationErrors};
use crate::graph::ir::{IrGraph, Node as GraphNode, NodeId, NodeInput as GraphNodeInput, NodeInputDep as GraphNodeInputDep, NodeInputWithLayout as GraphNodeInputWithLayout, NodeIOType};
use crate::raw::{ComputeFn, IOData, InputData, OutputData, NullRegion, RawData, ConstantPool};
use structural_reflection::{IsSubtypeOf, RustType};

/// Built (lowered) compound view graph, loaded from a `.dui` file.
///
/// This graph is completely valid. However, in order to run it, you must check the inputs and
/// outputs, as they may still be incompatible with the graph.
pub struct LowerGraph<RuntimeCtx: 'static + ?Sized> {
    input_types: Vec<NodeIOType>,
    output_types: Vec<NodeIOType>,
    compute_dag: Vec<Node<RuntimeCtx>>,
    outputs: Vec<NodeInput>,
    // This must be alive as there are retained raw pointers, even though this isn't used
    #[allow(dead_code)]
    constant_pool: ConstantPool,
    const_output_data: IOData
}

struct Node<RuntimeCtx: 'static + ?Sized> {
    compute: ComputeFn<RuntimeCtx>,
    inputs: Vec<NodeInput>,
    default_outputs: Vec<NodeInput>,
    cached_input_data: IOData,
    cached_output_data: IOData
}

enum NodeInput {
    Dep(NodeInputDep),
    Hole,
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

impl<RuntimeCtx: 'static + ?Sized> LowerGraph<RuntimeCtx> {
    pub unsafe fn try_from_but_assume_sorted_if_there_are_no_cycles(graph: IrGraph<RuntimeCtx>) -> Result<Self, GraphValidationErrors> {
        let errors = graph.validate();
        if errors.is_empty() {
            Ok(LowerGraph::new_unchecked(graph, true))
        } else {
            Err(errors)
        }
    }

    pub unsafe fn new_unchecked(graph: IrGraph<RuntimeCtx>, assume_deps_are_sorted: bool) -> Self {
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
        #[inline]
        unsafe fn get_input(input: GraphNodeInput, cached_input_data: &mut RawData, node_indices: &HashMap<NodeId, usize>, hole_is_valid: bool) -> NodeInput {
            match input {
                GraphNodeInput::Hole => {
                    assert!(hole_is_valid, "Hole input not valid");
                    NodeInput::Hole
                },
                GraphNodeInput::Dep(dep) => NodeInput::Dep(get_input_dep(dep, node_indices)),
                GraphNodeInput::ConstInline(const_) => {
                    debug_assert_eq!(const_.len(), cached_input_data.len());
                    copy_nonoverlapping(const_.as_ptr() as *const MaybeUninit<u8>, cached_input_data.as_mut_ptr(), cached_input_data.len());
                    NodeInput::Const
                },
                GraphNodeInput::ConstRef(ptr) => {
                    // We don't know pointer size, all we know is that it's at least as large as a thin pointer
                    debug_assert!(size_of::<*const ()>() <= cached_input_data.len());
                    copy_nonoverlapping(ptr.ptr_to_pointer_data() as *const MaybeUninit<u8>, cached_input_data.as_mut_ptr(), cached_input_data.len());
                    NodeInput::Const
                },
                GraphNodeInput::Array(inputs) => NodeInput::Array(inputs.into_iter().map(|input| get_input(input, cached_input_data, node_indices, hole_is_valid)).collect()),
                GraphNodeInput::Tuple(inputs_with_layouts) => NodeInput::Tuple(inputs_with_layouts.into_iter().map(|GraphNodeInputWithLayout { input, size, align }| NodeInputWithLayout { input: get_input(input, cached_input_data, node_indices, hole_is_valid), size, align }).collect())
            }
        }
        #[inline]
        unsafe fn get_inputs(input_types: &[NodeIOType], inputs: Vec<GraphNodeInput>, node_indices: &HashMap<NodeId, usize>, hole_is_valid: bool) -> (IOData, Vec<NodeInput>) {
            debug_assert!(input_types.len() == inputs.len());
            let mut cached_input_data = IOData::new(
                input_types.iter().map(|input| input.rust_type.clone()).collect::<Vec<_>>(),
                input_types.iter().map(|input| input.null_region.clone()).collect::<Vec<_>>(),
                repeat(null()).take(inputs.len())
            );
            let inputs = zip(inputs.into_iter(), cached_input_data.iter_data_mut())
                .map(|(input, cached_input_data)| get_input(input, cached_input_data, node_indices, hole_is_valid))
                .collect::<Vec<_>>();
            (cached_input_data, inputs)
        }
        let compute_dag = sorted_nodes.into_iter().map(|(_, node)| {
            let node_type = &graph.types[&node.type_name];

            debug_assert_eq!(node_type.inputs.len(), node.inputs.len());
            debug_assert_eq!(node_type.outputs.len(), node.default_outputs.len());

            let compute = node.compute.expect("graph should've been validated, but there is a node without compute");
            let (cached_input_data, inputs) = get_inputs(&node_type.inputs, node.inputs, &node_indices, false);
            let (cached_output_data, default_outputs) = get_inputs(&node_type.outputs, node.default_outputs, &node_indices, true);

            Node {
                compute,
                inputs,
                default_outputs,
                cached_input_data,
                cached_output_data
            }
        }).collect::<Vec<_>>();
        let (cached_output_data, outputs) = get_inputs(&output_types, graph.outputs, &node_indices, false);

        LowerGraph {
            input_types,
            output_types,
            compute_dag,
            outputs,
            constant_pool: graph.constant_pool,
            const_output_data: cached_output_data
        }
    }

    pub fn check(
        &self,
        input_types: &[RustType],
        output_types: &[RustType],
        input_regions: &[NullRegion],
        output_regions: &[NullRegion]
    ) -> GraphIOCheckErrors {
        debug_assert_eq!(input_types.len(), input_regions.len());
        debug_assert_eq!(output_types.len(), output_regions.len());

        let mut errors = GraphIOCheckErrors::new();

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
        for ((actual_type, actual_region), NodeIOType { name, rust_type: expected_type, null_region: expected_region }) in zip(zip(input_types.iter(), input_regions.iter()), self.input_types.iter()) {
            match actual_type.is_rough_subtype_of(expected_type) {
                IsSubtypeOf::No => {
                    errors.push(GraphIOCheckError::InputTypeMismatch {
                        field_name: name.clone(),
                        expected: expected_type.clone(),
                        actual: actual_type.clone()
                    });
                },
                IsSubtypeOf::Unknown => {
                    errors.push(GraphIOCheckError::InputTypeMaybeMismatch {
                        field_name: name.clone(),
                        expected: expected_type.clone(),
                        actual: actual_type.clone()
                    })
                },
                IsSubtypeOf::Yes => {}
            }
            if !actual_region.is_subset_of(expected_region) {
                errors.push(GraphIOCheckError::InputNullabilityMismatch {
                    field_name: name.clone(),
                    expected: expected_region.clone(),
                    actual: actual_region.clone()
                });
            }
        }
        for ((actual_type, actual_region), NodeIOType { name, rust_type: expected_type, null_region: expected_region }) in zip(zip(output_types.iter(), output_regions.iter()), self.output_types.iter()) {
            match expected_type.is_rough_subtype_of(actual_type) {
                IsSubtypeOf::No => {
                    errors.push(GraphIOCheckError::OutputTypeMismatch {
                        field_name: name.clone(),
                        expected: expected_type.clone(),
                        actual: actual_type.clone()
                    });
                },
                IsSubtypeOf::Unknown => {
                    errors.push(GraphIOCheckError::OutputTypeMaybeMismatch {
                        field_name: name.clone(),
                        expected: expected_type.clone(),
                        actual: actual_type.clone()
                    })
                },
                IsSubtypeOf::Yes => {}
            }
            if !expected_region.is_subset_of(actual_region) {
                errors.push(GraphIOCheckError::OutputNullabilityMismatch {
                    field_name: name.clone(),
                    expected: expected_region.clone(),
                    actual: actual_region.clone()
                });
            }
        }

        errors
    }

    pub fn compute(&mut self, ctx: &mut RuntimeCtx, inputs: &InputData, outputs: &mut OutputData) -> Result<(), GraphIOCheckErrors> {
        let errors = self.check(inputs.rust_types(), outputs.rust_types(), inputs.null_regions(), outputs.null_regions());
        if errors.is_empty() {
            Ok(unsafe { self.compute_unchecked(ctx, inputs, outputs) })
        } else {
            Err(errors)
        }
    }

    pub unsafe fn compute_unchecked(&mut self, ctx: &mut RuntimeCtx, inputs: &InputData, outputs: &mut OutputData) {
        debug_assert!(inputs.len() == self.input_types.len() && outputs.len() == self.output_types.len());

        #[inline]
        unsafe fn handle_input<RuntimeCtx: 'static + ?Sized>(input: &NodeInput, input_data: &mut RawData, graph_inputs: &InputData, compute_sub_dag: &[Node<RuntimeCtx>]) {
            match input {
                NodeInput::Hole | NodeInput::Const => {},
                NodeInput::Dep(dep) => {
                    let other_output = match dep {
                        NodeInputDep::OtherNodeOutput { node_idx, field_idx } => {
                            let other_node = &compute_sub_dag[*node_idx];
                            other_node.cached_output_data.data(*field_idx)
                        }
                        NodeInputDep::GraphInput { field_idx } => graph_inputs.data(*field_idx)
                    };
                    debug_assert!(other_output.len() == input_data.len());
                    input_data.copy_from_slice(other_output);
                    copy_nonoverlapping(other_output.as_ptr(), input_data.as_mut_ptr(), other_output.len());
                }
                NodeInput::Array(inputs) => {
                    let input_datas = input_data.chunks_mut(inputs.len());
                    for (input, input_data) in zip(inputs, input_datas) {
                        handle_input(input, input_data, graph_inputs, compute_sub_dag);
                    }
                }
                NodeInput::Tuple(inputs) => {
                    let mut offset = 0;
                    for NodeInputWithLayout { input, size, align } in inputs {
                        if offset % align != 0 {
                            offset += align - offset % align;
                        }
                        let input_data = &mut input_data[offset..offset + size];
                        handle_input(input, input_data, graph_inputs, compute_sub_dag);
                        offset += size;
                    }
                }
            }
        }
        #[inline]
        unsafe fn handle_inputs<RuntimeCtx: 'static + ?Sized>(inputs: &[NodeInput], inputs_data: &mut IOData, graph_inputs: &InputData, compute_sub_dag: &[Node<RuntimeCtx>]) {
            for (input, input_data) in zip(inputs, inputs_data.iter_data_mut()) {
                handle_input(&input, input_data, graph_inputs, compute_sub_dag);
            }
        }

        for index in 0..self.compute_dag.len() {
            let (node, compute_sub_dag) = self.compute_dag[..=index].split_last_mut().unwrap();
            // Copy input data to nodes (const data is already there), then compute to output data
            handle_inputs(&node.inputs, &mut node.cached_input_data, inputs, &compute_sub_dag);
            // Also copy default output data, which may be overridden by compute
            handle_inputs(&node.default_outputs, &mut node.cached_output_data, inputs, &compute_sub_dag);
            node.compute.call(ctx, node.cached_input_data.as_input(), node.cached_output_data.as_output())
        }

        // Copy const data, then write the rest
        // const data
        self.const_output_data.copy_data_into(outputs.as_raw());
        // the rest
        handle_inputs(&self.outputs, outputs.as_raw(), inputs, &self.compute_dag);
    }
}

impl<RuntimeCtx: 'static + ?Sized> TryFrom<IrGraph<RuntimeCtx>> for LowerGraph<RuntimeCtx> {
    type Error = GraphValidationErrors;

    fn try_from(value: IrGraph<RuntimeCtx>) -> Result<Self, Self::Error> {
        let errors = value.validate();
        if errors.is_empty() {
            Ok(unsafe { LowerGraph::new_unchecked(value, false) })
        } else {
            Err(errors)
        }
    }
}
