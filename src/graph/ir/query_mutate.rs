use std::cmp::Ordering;
use std::collections::HashSet;
use std::iter::zip;

use crate::error::{GraphValidationError, GraphValidationErrors, NodeCycle, NodeDisplayInputName};
use crate::ir::{IrGraph, Node, NodeId, NodeIO, NodeIODep, NodeIOType};
use structural_reflection::{IsSubtypeOf, RustType};
use crate::raw::Nullability;

impl<RuntimeCtx: 'static + ?Sized> IrGraph<RuntimeCtx> {
    pub fn validate(&self) -> GraphValidationErrors {
        let mut errors = GraphValidationErrors::new();

        if let Err(cycle) = self.check_cycle() {
            errors.push(GraphValidationError::Cycle(cycle))
        }

        // Check compute
        for (node_id, node) in self.iter_nodes() {
            if node.compute.is_none() {
                errors.push(GraphValidationError::NoCompute {
                    node: node.display(node_id)
                });
            }
        }

        // check input and output types of each node, and nullability (counting nullable with default values as not nullable)
        for (node_id, node) in self.iter_nodes() {
            let node_type = &self.types[&node.type_name];
            let inputs = &node.inputs;
            let input_types = &node_type.inputs;
            let default_outputs = &node.default_outputs;
            let output_types = &node_type.outputs;

            assert_eq!(inputs.len(), input_types.len(), "input count mismatch");
            assert_eq!(default_outputs.len(), output_types.len(), "output count mismatch");

            for (input, NodeIOType { name: input_name, rust_type: input_rust_type, nullability: input_nullability }) in zip(inputs, input_types) {
                self.check_type(&mut errors, node_id, node, input, input_name, input_rust_type, input_nullability);
            }
            for (default_output, NodeIOType { name: output_name, rust_type: output_rust_type, nullability: _ }) in zip(default_outputs, output_types) {
                // input nullability is null because the default output is never required, even if the actual type is non-null
                self.check_type(&mut errors, node_id, node, default_output, output_name, output_rust_type, &Nullability::Null);
            }
        }

        errors
    }

    fn check_type(
        &self,
        errors: &mut GraphValidationErrors,
        node_id: NodeId,
        node: &Node<RuntimeCtx>,
        input: &NodeIO,
        input_name: &str,
        input_rust_type: &RustType,
        input_nullability: &Nullability,
    ) {
        self.check_type_itself(
            errors,
            node_id,
            node,
            input_name,
            input_rust_type
        );
        match input {
            NodeIO::Hole => if !Nullability::Null.is_subset_of(input_nullability) {
                errors.push(GraphValidationError::IONullabilityMismatch {
                    output_nullability: Nullability::Null,
                    input_nullability: input_nullability.clone(),
                    referenced_from: NodeDisplayInputName {
                        node: node.display(node_id),
                        input_name: input_name.to_string()
                    }
                });
                // Type is ok (bottom)
            },
            NodeIO::Dep(dep) => {
                let output_type = match dep {
                    NodeIODep::GraphInput { idx} => &self.input_types[*idx],
                    NodeIODep::OtherNodeOutput { id, idx } => &self.types[&self.nodes[id.0].type_name].outputs[*idx]
                };
                let default_output = match dep {
                    NodeIODep::GraphInput { idx} => &self.default_inputs[*idx],
                    NodeIODep::OtherNodeOutput { id, idx } => &self.nodes[id.0].default_outputs[*idx]
                };
                let NodeIOType { name: _, rust_type: output_rust_type, nullability: output_type_nullability } = output_type;
                let mut output_nullability = default_output.nullability(self);
                output_nullability.intersect(output_type_nullability);

                self.check_type2(errors, node_id, node, input_name, input_rust_type, input_nullability, output_rust_type, &output_nullability)
            }
            NodeIO::ConstInline(_) | NodeIO::ConstRef(_) => {
                // Nullability is ok (not nullable)
                // Type is ok (invariant)
            }
            NodeIO::Array(elems) => {
                let (elem_input_type, len) = input_rust_type.structure.array_elem_type_and_length().expect("broken invariant: node input is array but type isn't array");
                assert_eq!(elems.len(), len, "broken invariant: node input / type array length mismatch");
                let elem_input_nullability = input_nullability;
                for elem in elems {
                    self.check_type(errors, node_id, node, elem, input_name, elem_input_type, elem_input_nullability);
                }
            }
            NodeIO::Tuple(elems) => {
                let elem_input_types = input_rust_type.structure.general_compound_elem_types().expect("broken invariant: node input is tuple but type isn't tuple");
                let elem_input_nullabilities = input_nullability.subdivide();
                for (elem, (elem_input_type, elem_input_nullability)) in zip(elems, zip(elem_input_types, elem_input_nullabilities)) {
                    self.check_type(errors, node_id, node, &elem.input, input_name, elem_input_type, elem_input_nullability);
                }
            }
        }
    }

    fn check_type2(
        &self,
        errors: &mut GraphValidationErrors,
        node_id: NodeId,
        node: &Node<RuntimeCtx>,
        input_name: &str,
        input_rust_type: &RustType,
        input_nullability: &Nullability,
        output_rust_type: &RustType,
        output_nullability: &Nullability,
    ) {
        match output_rust_type.is_rough_subtype_of(input_rust_type) {
            IsSubtypeOf::No => {
                errors.push(GraphValidationError::IOTypeMismatch {
                    output_type: input_rust_type.clone(),
                    input_type: output_rust_type.clone(),
                    referenced_from: NodeDisplayInputName {
                        node: node.display(node_id),
                        input_name: input_name.to_string()
                    }
                });
            },
            IsSubtypeOf::Unknown => {
                errors.push(GraphValidationError::IOTypeMaybeMismatch {
                    output_type: input_rust_type.clone(),
                    input_type: output_rust_type.clone(),
                    referenced_from: NodeDisplayInputName {
                        node: node.display(node_id),
                        input_name: input_name.to_string()
                    }
                });
            }
            IsSubtypeOf::Yes => {}
        }
        if !output_nullability.is_subset_of(input_nullability) {
            errors.push(GraphValidationError::IONullabilityMismatch {
                output_nullability: input_nullability.clone(),
                input_nullability: output_nullability.clone(),
                referenced_from: NodeDisplayInputName {
                    node: node.display(node_id),
                    input_name: input_name.to_string()
                }
            });
        }
    }

    fn check_type_itself(
        &self,
        errors: &mut GraphValidationErrors,
        node_id: NodeId,
        node: &Node<RuntimeCtx>,
        input_name: &str,
        rust_type: &RustType
    ) {
        if rust_type.size == usize::MAX {
            errors.push(GraphValidationError::UnknownSizedType {
                type_: rust_type.clone(),
                referenced_from: NodeDisplayInputName {
                    node: node.display(node_id),
                    input_name: input_name.to_string()
                }
            })
        }
        if rust_type.align == usize::MAX {
            errors.push(GraphValidationError::UnknownAlignedType {
                type_: rust_type.clone(),
                referenced_from: NodeDisplayInputName {
                    node: node.display(node_id),
                    input_name: input_name.to_string()
                }
            })
        }
    }

    pub fn iter_node_ids(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.nodes.iter().map(|(id, _)| NodeId(id))
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item=(NodeId, &Node<RuntimeCtx>)> {
        self.nodes.iter().map(|(id, node)| (NodeId(id), node))
    }

    pub fn iter_mut_nodes(&mut self) -> impl Iterator<Item=(NodeId, &mut Node<RuntimeCtx>)> {
        self.nodes.iter_mut().map(|(id, node)| (NodeId(id), node))
    }

    /// Doesn't iterate nested types. If you want to get type names you don't need to iterate nested
    /// rust types or structures, because any type names are only in the surface types
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&RustType> {
        (self.input_types.iter().chain(self.output_types.iter()))
            .map(|io_type| &io_type.rust_type)
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

impl<RuntimeCtx: 'static + ?Sized> Node<RuntimeCtx> {
    pub fn depends_on(&self, other_id: NodeId) -> bool {
        self.iter_dep_nodes().any(|dep| dep == other_id)
    }

    pub fn iter_deps(&self) -> impl Iterator<Item=NodeIODep> + '_ {
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

impl NodeIO {
    pub const ZST: NodeIO = NodeIO::Tuple(Vec::new());

    pub fn deps(&self) -> impl Iterator<Item=NodeIODep> + '_ {
        match &self {
            NodeIO::Hole | NodeIO::ConstInline(_) | NodeIO::ConstRef(_) => Box::new(std::iter::empty()) as Box<dyn Iterator<Item=NodeIODep>>,
            NodeIO::Dep(dep) => Box::new(std::iter::once(*dep)) as Box<dyn Iterator<Item=NodeIODep>>,
            NodeIO::Array(inputs) => Box::new(inputs.iter().flat_map(|input| input.deps())) as Box<dyn Iterator<Item=NodeIODep>>,
            NodeIO::Tuple(inputs_with_layouts) => Box::new(inputs_with_layouts.iter().flat_map(|input_with_layout| input_with_layout.input.deps())) as Box<dyn Iterator<Item=NodeIODep>>
        }
    }

    pub fn dep_nodes(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.deps().filter_map(|dep| match dep {
            NodeIODep::GraphInput { idx: _ } => None,
            NodeIODep::OtherNodeOutput { id, idx: _ } => Some(id)
        })
    }

    fn nullability<RuntimeCtx: 'static + ?Sized>(&self, graph: &IrGraph<RuntimeCtx>) -> Nullability {
        match &self {
            NodeIO::Hole => Nullability::Null,
            NodeIO::ConstInline(_) | NodeIO::ConstRef(_) => Nullability::NonNull,
            NodeIO::Dep(dep) => {
                let output_type = match dep {
                    NodeIODep::GraphInput { idx} => &graph.input_types[*idx],
                    NodeIODep::OtherNodeOutput { id, idx } => &graph.types[&graph.nodes[id.0].type_name].outputs[*idx]
                };
                let default_output = match dep {
                    NodeIODep::GraphInput { idx} => &graph.default_inputs[*idx],
                    NodeIODep::OtherNodeOutput { id, idx } => &graph.nodes[id.0].default_outputs[*idx]
                };
                let mut output_nullability = default_output.nullability(graph);
                output_nullability.intersect(&output_type.nullability);
                output_nullability
            },
            NodeIO::Array(inputs) => Nullability::Partial(inputs.iter().map(|input| input.nullability(graph)).collect()),
            NodeIO::Tuple(inputs_with_layouts) => Nullability::Partial(inputs_with_layouts.iter().map(|input_with_layout| input_with_layout.input.nullability(graph)).collect())
        }
    }
}