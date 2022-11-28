use std::collections::HashMap;
use std::iter::zip;
use std::mem::{MaybeUninit, size_of};
use std::ptr::copy_nonoverlapping;
use crate::graph::error::{GraphIOCheckError, GraphIOCheckErrors, GraphValidationErrors};
use crate::graph::ir::{IrGraph, Node as GraphNode, NodeId, NodeIO as GraphNodeInput, NodeIODep as GraphNodeInputDep, NodeIOWithLayout as GraphNodeInputWithLayout, NodeIOType};
use crate::raw::{ComputeFn, IOData, StoreData, LoadData, Nullability, RawData, ConstantPool, IOTypeCheckError};
use structural_reflection::{align_up, IsSubtypeOf, RustType};

/// Lower graph with its own input and output data.
/// By default it is not self-contained because that is an extra copy.
pub struct SelfContainedLowerGraph<RuntimeCtx: 'static + ?Sized> {
    pub graph: LowerGraph<RuntimeCtx>,
    pub input_data: IOData,
    pub output_data: IOData,
}

impl<RuntimeCtx: 'static + ?Sized> SelfContainedLowerGraph<RuntimeCtx> {
    pub fn new(graph: LowerGraph<RuntimeCtx>) -> Self {
        let input_data = graph.mk_uninit_input_data();
        let output_data = graph.mk_uninit_output_data();
        SelfContainedLowerGraph {
            graph,
            input_data,
            output_data,
        }
    }
}

/// Built (lowered) compound view graph, loaded from a `.dvl` file.
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
    default_input_data: IOData,
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
        unsafe fn get_input(input: GraphNodeInput, (cached_input_data, cached_input_raw_nullability): (&mut RawData, &mut Nullability), node_indices: &HashMap<NodeId, usize>, hole_is_valid: bool) -> NodeInput {
            match input {
                GraphNodeInput::Hole => {
                    assert!(hole_is_valid, "Hole input not valid");
                    NodeInput::Hole
                },
                GraphNodeInput::Dep(dep) => {
                    *cached_input_raw_nullability = Nullability::NonNull;
                    NodeInput::Dep(get_input_dep(dep, node_indices))
                },
                GraphNodeInput::ConstInline(const_) => {
                    *cached_input_raw_nullability = Nullability::NonNull;
                    debug_assert_eq!(const_.len(), cached_input_data.len());
                    copy_nonoverlapping(const_.as_ptr() as *const MaybeUninit<u8>, cached_input_data.as_mut_ptr(), cached_input_data.len());
                    NodeInput::Const
                },
                GraphNodeInput::ConstRef(ptr) => {
                    *cached_input_raw_nullability = Nullability::NonNull;
                    // We don't know pointer size, all we know is that it's at least as large as a thin pointer
                    debug_assert!(size_of::<*const ()>() <= cached_input_data.len());
                    copy_nonoverlapping(ptr.ptr_to_pointer_data() as *const MaybeUninit<u8>, cached_input_data.as_mut_ptr(), cached_input_data.len());
                    NodeInput::Const
                },
                GraphNodeInput::Array(inputs) => {
                    let elem_aligned_size = cached_input_data.len() / inputs.len();
                    let (lowered_inputs, raw_nullabilities) = zip(inputs, cached_input_data.chunks_exact_mut(elem_aligned_size)).map(|(input, cached_input_data)| {
                        let mut cached_input_raw_nullability = Nullability::Null;
                        let lowered_input = get_input(input, (cached_input_data, &mut cached_input_raw_nullability), node_indices, hole_is_valid);
                        (lowered_input, cached_input_raw_nullability)
                    }).unzip();
                    *cached_input_raw_nullability = Nullability::Partial(raw_nullabilities);
                    NodeInput::Array(lowered_inputs)
                },
                GraphNodeInput::Tuple(inputs_with_layouts) => {
                    let mut offset = 0;
                    let (lowered_inputs, raw_nullabilities) = inputs_with_layouts.into_iter().map(|GraphNodeInputWithLayout { input, size, align }| {
                        let mut cached_input_raw_nullability = Nullability::Null;
                        offset = align_up(offset, align);
                        let cached_input_data = &mut cached_input_data[offset..offset + size];
                        let lowered_input = NodeInputWithLayout {
                            input: get_input(input, (cached_input_data, &mut cached_input_raw_nullability), node_indices, hole_is_valid),
                            size,
                            align
                        };
                        offset += size;
                        (lowered_input, cached_input_raw_nullability)
                    }).unzip();
                    *cached_input_raw_nullability = Nullability::Partial(raw_nullabilities);
                    NodeInput::Tuple(lowered_inputs)
                }
            }
        }
        #[inline]
        unsafe fn get_inputs(input_types: &[NodeIOType], inputs: Vec<GraphNodeInput>, node_indices: &HashMap<NodeId, usize>, hole_is_valid: bool) -> (IOData, Vec<NodeInput>) {
            debug_assert!(input_types.len() == inputs.len());
            let mut cached_input_data = IOData::new_raw_uninit(
                input_types.iter().map(|input| input.rust_type.clone()).collect::<Vec<_>>(),
                input_types.iter().map(|input| input.nullability.clone()).collect::<Vec<_>>()
            );
            let inputs = zip(inputs.into_iter(), cached_input_data.iter_raw_data_mut())
                .map(|(input, cached_input_data_and_nullability)| get_input(input, cached_input_data_and_nullability, node_indices, hole_is_valid))
                .collect::<Vec<_>>();
            (cached_input_data, inputs)
        }

        // default_inputs should all be const, holes, or collections, and we don't use them
        let (cached_default_input_data, default_inputs) = get_inputs(&input_types, graph.default_inputs, &node_indices, true);
        debug_assert!(
            default_inputs.iter().all(|default_input| !matches!(default_input, NodeInput::Dep(_))),
            "default inputs can't be deps (should already be detected as a cycle)"
        );
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
            default_input_data: cached_default_input_data,
            const_output_data: cached_output_data
        }
    }

    pub fn check(
        &self,
        input_types: &[RustType],
        output_types: &[RustType],
        input_type_nullabilities: &[Nullability],
        input_value_nullabilities: &[Nullability],
        output_nullabilities: &[Nullability]
    ) -> GraphIOCheckErrors {
        debug_assert_eq!(input_value_nullabilities.len(), input_type_nullabilities.len());
        debug_assert_eq!(input_types.len(), input_value_nullabilities.len());
        debug_assert_eq!(output_types.len(), output_nullabilities.len());

        let input_value_nullabilities = || zip(
            input_value_nullabilities,
            self.default_input_data.raw_nullabilities()
        ).map(|(value_nullability, default_nullability)| {
            let mut nullability = value_nullability.clone();
            nullability.intersect(default_nullability);
            nullability
        });

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
        for (index, (input_valueability, input_typeability)) in zip(input_value_nullabilities(), input_type_nullabilities).enumerate() {
            if !input_valueability.is_subset_of(input_typeability) {
                errors.push(GraphIOCheckError::IOTypeCheckError(IOTypeCheckError::InvalidRawNullability {
                    index,
                    actual: input_valueability,
                    expected: input_typeability.clone()
                }));
            }
        }
        for ((actual_type, actualability), NodeIOType { name, rust_type: expected_type, nullability: expectedability }) in zip(zip(input_types, input_value_nullabilities()), &self.input_types) {
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
            if !actualability.is_subset_of(expectedability) {
                errors.push(GraphIOCheckError::InputNullabilityMismatch {
                    field_name: name.clone(),
                    expected: expectedability.clone(),
                    actual: actualability
                });
            }
        }
        for ((actual_type, actualability), NodeIOType { name, rust_type: expected_type, nullability: expectedability }) in zip(zip(output_types.iter(), output_nullabilities.iter()), self.output_types.iter()) {
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
            if !expectedability.is_subset_of(actualability) {
                errors.push(GraphIOCheckError::OutputNullabilityMismatch {
                    field_name: name.clone(),
                    expected: expectedability.clone(),
                    actual: actualability.clone()
                });
            }
        }

        errors
    }

    /// Creates a buffer of uninitialized data of the type required for this graph's input
    pub fn mk_uninit_input_data(&self) -> IOData {
        IOData::new_raw_uninit(
            self.input_types.iter().map(|input_type| input_type.rust_type.clone()).collect(),
            self.input_types.iter().map(|input_type| input_type.nullability.clone()).collect()
        )
    }

    /// Creates a buffer of uninitialized data of the type required for this graph's output
    pub fn mk_uninit_output_data(&self) -> IOData {
        IOData::new_raw_uninit(
            self.output_types.iter().map(|output_type| output_type.rust_type.clone()).collect(),
            self.output_types.iter().map(|output_type| output_type.nullability.clone()).collect()
        )
    }

    pub fn compute(&mut self, ctx: &mut RuntimeCtx, inputs: &LoadData, outputs: &mut StoreData) -> Result<(), GraphIOCheckErrors> {
        let errors = self.check(
            inputs.rust_types(),
            outputs.rust_types(),
            inputs.type_nullabilities(),
            inputs.raw_nullabilities(),
            outputs.type_nullabilities()
        );
        if errors.is_empty() {
            Ok(unsafe { self.compute_unchecked(ctx, inputs, outputs) })
        } else {
            Err(errors)
        }
    }

    pub unsafe fn compute_unchecked(&mut self, ctx: &mut RuntimeCtx, inputs: &LoadData, outputs: &mut StoreData) {
        debug_assert!(inputs.len() == self.input_types.len() && outputs.len() == self.output_types.len());

        #[inline]
        unsafe fn handle_input<RuntimeCtx: 'static + ?Sized>(
            input: &NodeInput,
            (input_data, input_nullability): (&mut RawData, &mut Nullability),
            graph_inputs: &IOData,
            default_graph_inputs: &IOData,
            compute_sub_dag: &[Node<RuntimeCtx>]
        ) {
            match input {
                NodeInput::Hole | NodeInput::Const => {},
                NodeInput::Dep(dep) => {
                    let (other_output, other_nullability) = match dep {
                        NodeInputDep::OtherNodeOutput { node_idx, field_idx } => {
                            let other_node = &compute_sub_dag[*node_idx];
                            other_node.cached_output_data.raw_data_idx(*field_idx)
                        }
                        NodeInputDep::GraphInput { field_idx } => {
                            if !graph_inputs.raw_nullabilities()[*field_idx].is_subset_of(&default_graph_inputs.raw_nullabilities()[*field_idx]) {
                                if !matches!(default_graph_inputs.raw_nullabilities()[*field_idx], Nullability::NonNull) {
                                    unimplemented!("default partial input where only some of the data is overridden");
                                }
                                default_graph_inputs.raw_data_idx(*field_idx)
                            } else {
                                graph_inputs.raw_data_idx(*field_idx)
                            }
                        }
                    };
                    input_data.copy_from_slice(other_output);
                    other_nullability.clone_into(input_nullability);
                }
                NodeInput::Array(inputs) => {
                    let input_datas = input_data.chunks_mut(inputs.len());
                    for (input, input_data_and_nullability) in zip(inputs, zip(input_datas, input_nullability.subdivide_mut(inputs.len()))) {
                        handle_input(input, input_data_and_nullability, graph_inputs, default_graph_inputs, compute_sub_dag);
                    }
                }
                NodeInput::Tuple(inputs) => {
                    let mut offset = 0;
                    for (NodeInputWithLayout { input, size, align }, input_nullability) in zip(inputs, input_nullability.subdivide_mut(inputs.len())) {
                        if offset % align != 0 {
                            offset += align - offset % align;
                        }
                        let input_data = &mut input_data[offset..offset + size];
                        handle_input(input, (input_data, input_nullability), graph_inputs, default_graph_inputs, compute_sub_dag);
                        offset += size;
                    }
                }
            }
        }
        #[inline]
        unsafe fn handle_inputs<RuntimeCtx: 'static + ?Sized>(
            inputs: &[NodeInput],
            inputs_data: &mut IOData,
            graph_inputs: &IOData,
            default_graph_inputs: &IOData,
            compute_sub_dag: &[Node<RuntimeCtx>]
        ) {
            for (input, input_data_and_nullability) in zip(inputs, inputs_data.iter_raw_data_mut()) {
                handle_input(&input, input_data_and_nullability, graph_inputs, default_graph_inputs, compute_sub_dag);
            }
        }

        for index in 0..self.compute_dag.len() {
            let (node, compute_sub_dag) = self.compute_dag[..=index].split_last_mut().unwrap();
            // Copy input data to nodes (const data is already there), then compute to output data
            handle_inputs(&node.inputs, &mut node.cached_input_data, &inputs.0, &self.default_input_data, &compute_sub_dag);
            // Also copy default output data, which may be overridden by compute
            handle_inputs(&node.default_outputs, &mut node.cached_output_data, &inputs.0, &self.default_input_data, &compute_sub_dag);
            node.compute.call(ctx, node.cached_input_data.read_only(), node.cached_output_data.write_only())
        }

        // Copy const data, then write the rest
        // const data
        self.const_output_data.copy_data_into_unchecked(&mut outputs.0);
        // the rest
        handle_inputs(&self.outputs, &mut outputs.0, &inputs.0, &self.default_input_data, &self.compute_dag);
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