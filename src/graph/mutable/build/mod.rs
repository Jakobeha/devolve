use std::collections::{HashMap, HashSet};
use std::iter::zip;
use std::mem::{align_of, size_of, take};

use slab::Slab;

use crate::graph::builtins::{BuiltinNodeType, BuiltinNodeTypeFnCtx};
use crate::graph::error::{GraphFormError, GraphFormErrors, NodeNameFieldName};
use crate::graph::mutable::{FieldHeader, MutableGraph, Node, NodeId, NodeInput, NodeInputDep, NodeInputWithLayout, NodeIOType, NodeMetadata, NodeTypeData, NodeTypeName};
use crate::graph::mutable::build::size_and_align::{calculate_align, calculate_array_size, calculate_size};
use crate::graph::parse::topological_sort::SortByDeps;
//noinspection RsUnusedImport (intelliJ fails to see SerialFieldElem use)
use crate::graph::parse::types::{SerialBody, SerialEnumType, SerialEnumVariantType, SerialField, SerialFieldElem, SerialFieldType, SerialGraph, SerialNode, SerialRustType, SerialStructType, SerialType, SerialTypeBody, SerialValueHead};
use crate::graph::raw::RawComputeFn;
use crate::misc::map_box::map_box;
use crate::rust_type::{infer_tuple_align, infer_tuple_size, IsSubtypeOf, KnownRustType, RustType, StructuralRustType, TypeEnumVariant, TypeStructBody, TypeStructField, TypeStructure};

mod size_and_align;

pub(super) struct GraphBuilder<'a> {
    errors: &'a mut GraphFormErrors,
    graph_types: HashSet<String>,
    resolved_rust_types: HashMap<String, StructuralRustType>,
    resolved_node_types: HashMap<NodeTypeName, NodeTypeData>,
    resolved_nodes: HashMap<String, (NodeId, Node)>,
}

impl<'a> GraphBuilder<'a> {
    /// Note: the built graph's nodes are guaranteed to be topologically sorted as long as there are no cycles.
    /// This is not the case for [MutableGraph] in general though.
    pub(super) fn build(graph: SerialGraph, errors: &'a mut GraphFormErrors) -> MutableGraph {
        GraphBuilder {
            errors,
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

    fn resolve_type_def(&mut self, name: &str, type_def: SerialType) -> StructuralRustType {
        match type_def {
            SerialType::Struct(struct_type) => self.resolve_struct_type(name, struct_type),
            SerialType::Enum(enum_type) => self.resolve_enum_type(name, enum_type)
        }
    }

    fn resolve_struct_type(&mut self, name: &str, struct_type: SerialStructType) -> StructuralRustType {
        let structure = TypeStructure::CReprStruct {
            body: self.resolve_type_body(struct_type.body)
        };
        StructuralRustType {
            type_name: name.to_string(),
            size: structure.infer_size(),
            align: structure.infer_align(),
            structure,
        }
    }

    fn resolve_enum_type(&mut self, name: &str, enum_type: SerialEnumType) -> StructuralRustType {
        let structure = TypeStructure::CReprEnum {
            variants: enum_type.variants.into_iter().map(|variant| self.resolve_variant_type(variant)).collect()
        };
        StructuralRustType {
            type_name: name.to_string(),
            size: structure.infer_size(),
            align: structure.infer_align(),
            structure,
        }
    }

    fn resolve_variant_type(&mut self, variant_type: SerialEnumVariantType) -> TypeEnumVariant {
        TypeEnumVariant {
            name: variant_type.name,
            body: self.resolve_type_body(variant_type.body)
        }
    }

    fn resolve_type_body(&mut self, type_body: SerialTypeBody) -> TypeStructBody {
        match type_body {
            SerialTypeBody::None => TypeStructBody::None,
            SerialTypeBody::Tuple(elements) => TypeStructBody::Tuple(
                elements.into_iter().map(|item| self.resolve_type2(item)).collect(),
            ),
            SerialTypeBody::Fields(fields) => TypeStructBody::Fields(
                fields.into_iter().map(|item| self.resolve_field(item)).collect(),
            )
        }
    }

    fn resolve_field(&mut self, field: SerialFieldType) -> TypeStructField {
        TypeStructField {
            name: field.name,
            rust_type: self.resolve_type3(field.rust_type),
        }
    }

    fn resolve_node(&mut self, node_name: &str, node: SerialNode) -> (NodeTypeData, Node) {
        let inherited_type = node.node_type.as_ref()
            .and_then(|node_type| self.resolve_node_type(node_type, node_name));

        let (defined_input_types, defined_inputs, input_headers) = self.resolve_field_elems(node.input_fields, node_name);
        let (defined_output_types, defined_outputs, output_headers) = self.resolve_field_elems(node.output_fields, node_name);

        let output_idxs_with_values = defined_outputs.iter().enumerate().filter(|(_, output_value)| !matches!(output_value, NodeInput::Hole)).map(|(idx, _)| idx).collect::<Vec<_>>();
        for output_idx in output_idxs_with_values {
            let output_name = defined_output_types[output_idx].name.clone();
            self.errors.push(GraphFormError::OutputHasValue {
                node_name: node_name.to_string(),
                output_name
            });
        }

        let mut inputs = defined_inputs;

        let (node_type_name, type_data, compute) = match inherited_type {
            None => {
                // Use structural self-type
                let self_type_data = NodeTypeData {
                    inputs: defined_input_types,
                    outputs: defined_output_types
                };
                (NodeTypeName::from(node_name.to_string()), self_type_data, RawComputeFn::panicking())
            },
            Some(inherited_type) => {
                // Rearrange inputs and fill with holes, to match inherited type (there are no outputs)
                let mut old_inputs = Vec::new();
                old_inputs.append(&mut inputs);
                for input_io_type in inherited_type.type_data.inputs.iter() {
                    let input_field_name = &input_io_type.name;
                    let input_idx = defined_input_types.iter().position(|input_type| &input_type.name == input_field_name);

                    inputs.push(match input_idx {
                        None => NodeInput::Hole,
                        Some(input_idx) => take(&mut old_inputs[input_idx])
                    });
                }

                // Add default inputs which were not overridden
                for (input, default_input) in zip(inputs.iter_mut(), inherited_type.default_inputs.iter()) {
                    if matches!(input, NodeInput::Hole) {
                        // Add this default input since it's not overridden
                        *input = default_input.clone();
                    }
                }

                // Use inherited type
                let inherited_type_name = NodeTypeName::from(node.node_type.unwrap());
                let inherited_type_data = inherited_type.type_data.clone();
                (inherited_type_name, inherited_type_data, inherited_type.compute.clone())
            }
        };

        let meta = NodeMetadata {
            node_name: node_name.to_string(),
            input_headers,
            output_headers
        };

        let node = Node {
            type_name: node_type_name,
            inputs,
            compute,
            meta
        };

        (type_data, node)
    }

    fn resolve_node_type(&mut self, type_name: &str, node_name: &str) -> Option<BuiltinNodeType> {
        if let Some((fn_name, fn_arg)) = type_name.split_once('(') {
            let fn_arg = fn_arg.strip_suffix(')').unwrap_or_else(|| {
                self.errors.push(GraphFormError::NodeTypeFunctionMissingRParen { node_name: node_name.to_string() });
                fn_arg
            });
            self.resolve_node_type_fn(fn_name, fn_arg, node_name)
        } else {
            self.resolve_node_type_const(type_name, node_name)
        }
    }

    fn resolve_node_type_const(&mut self, type_name: &str, node_name: &str) -> Option<BuiltinNodeType> {
        match self.resolved_rust_types.get(type_name) {
            None => match BuiltinNodeType::get(type_name) {
                None => {
                    self.errors.push(GraphFormError::NodeTypeNotFound {
                        type_name: type_name.to_string(),
                        node_name: node_name.to_string()
                    });
                    None
                },
                Some(builtin) => Some(builtin)
            }
            Some(_) => {
                self.errors.push(GraphFormError::NodeIsDataType {
                    type_name: type_name.to_string(),
                    node_name: node_name.to_string()
                });
                None
            },
        }
    }

    fn resolve_node_type_fn(&mut self, fn_name: &str, fn_arg: &str, node_name: &str) -> Option<BuiltinNodeType> {
        match BuiltinNodeType::get_and_call_fn(fn_name, fn_arg, self.builtin_node_type_fn_ctx()) {
            None => {
                self.errors.push(GraphFormError::NodeTypeFunctionNotFound {
                    type_fn_name: fn_name.to_string(),
                    node_name: node_name.to_string()
                });
                None
            },
            Some(Err(error)) => {
                self.errors.push(GraphFormError::NodeTypeFunctionError {
                    error,
                    node_name: node_name.to_string()
                });
                None
            },
            Some(Ok(builtin)) => Some(builtin)
        }
    }

    fn builtin_node_type_fn_ctx(&self) -> BuiltinNodeTypeFnCtx<'_> {
        BuiltinNodeTypeFnCtx {
            resolved_rust_types: &self.resolved_rust_types
        }
    }

    fn resolve_field_elems(&mut self, fields: Vec<SerialFieldElem>, node_name: &str) -> (Vec<NodeIOType>, Vec<NodeInput>, Vec<FieldHeader>) {
        let mut defined_types: Vec<NodeIOType> = Vec::new();
        let mut defined_values: Vec<NodeInput> = Vec::new();
        let mut headers: Vec<FieldHeader> = Vec::new();
        for (index, field) in fields.into_iter().enumerate() {
            match field {
                SerialFieldElem::Header { header } => {
                    headers.push(FieldHeader { index, header });
                }
                SerialFieldElem::Field(field) => {
                    let (defined_input_type, defined_input) = self.resolve_node_field(field, node_name);
                    defined_types.push(defined_input_type);
                    defined_values.push(defined_input);
                }
            }
        }
        (defined_types, defined_values, headers)
    }

    fn resolve_node_field(&mut self, field: SerialField, node_name: &str) -> (NodeIOType, NodeInput) {
        let rust_type = self.resolve_type(
            field.rust_type,
            (field.value.as_ref(), &field.value_children),
        );
        let value = self.resolve_value((field.value, field.value_children), &rust_type, node_name, &field.name);
        let io_type = NodeIOType {
            name: field.name,
            rust_type
        };
        (io_type, value)
    }

    fn resolve_type(
        &mut self,
        serial_type: Option<SerialRustType>,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> RustType {
        let structural_type = self.resolve_type_structurally(serial_type, (value, value_children));
        self.further_resolve_structural_type(structural_type)
    }

    fn resolve_type2(&mut self, serial_type: SerialRustType) -> RustType {
        let structural_type = self.resolve_type_structurally2(serial_type);
        self.further_resolve_structural_type(structural_type)
    }

    fn resolve_type3(&mut self, serial_type: Option<SerialRustType>) -> RustType {
        match serial_type {
            None => RustType::unknown(),
            Some(serial_type) => self.resolve_type2(serial_type)
        }
    }

    fn further_resolve_structural_type(&mut self, structural_type: StructuralRustType) -> RustType {
        match KnownRustType::get(&structural_type.type_name) {
            None => RustType::Structural(structural_type),
            Some(known_type) => {
                if structural_type.structure.is_structural_subtype_of(&known_type.structure) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::TypeConflictsWithBuiltinType {
                        name: structural_type.type_name.to_string()
                    });
                }
                RustType::Known(known_type)
            }
        }
    }

    fn resolve_type_structurally(
        &mut self,
        serial_type: Option<SerialRustType>,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> StructuralRustType {
        let resolved_type = serial_type.map(|serial_type| self.resolve_type_structurally2(serial_type));
        let inferred_type = self.infer_type_structurally((value, value_children));
        self.merge_resolved_type(resolved_type, inferred_type)
    }

    fn resolve_type_structurally2(&mut self, serial_type: SerialRustType) -> StructuralRustType {
        let type_name = serial_type.to_string();
        let (known_size, known_align) = match &serial_type {
            SerialRustType::Ident { name, generic_args } => {
                let (previously_defined_size, previously_defined_align) = match self.resolved_rust_types.get(name.as_str()) {
                    None => (None, None),
                    Some(previously_defined_type) => {
                        (previously_defined_type.size, previously_defined_type.align)
                    }
                };
                let (intrinsic_size, intrinsic_align) = {
                    let name_with_generics = serial_type.to_string();
                    match KnownRustType::get(&name_with_generics) {
                        None => (None, None),
                        Some(known_type) => (Some(known_type.intrinsic.size), Some(known_type.intrinsic.align))
                    }
                };
                // Support simple case of recursively-defined types
                let (box_size, box_align) = if (name == "Box" || name == "Vec" || name == "HashSet") && generic_args.len() == 1 {
                    match &generic_args[0] {
                        SerialRustType::Ident { name, generic_args: _ } => {
                            if self.graph_types.contains(name.as_str()) {
                                match name.as_str() {
                                    "Box" => (Some(size_of::<Box<()>>()), Some(align_of::<Box<()>>())),
                                    "Vec" => (Some(size_of::<Vec<()>>()), Some(align_of::<Vec<()>>())),
                                    "HashSet" => (Some(size_of::<HashSet<()>>()), Some(align_of::<HashSet<()>>())),
                                    _ => unreachable!()
                                }
                            } else {
                                (None, None)
                            }
                        }
                        _ => (None, None)
                    }
                } else {
                    (None, None)
                };
                (
                    box_size.or(intrinsic_size).or(previously_defined_size),
                    box_align.or(intrinsic_align).or(previously_defined_align)
                )
            }
            _ => (None, None)
        };
        let structure = match serial_type {
            // Even if we defined the structure elsewhere, don't need to include it here
            // too much work and not actually necessary because we infer the size and alignment separately
            SerialRustType::Ident { .. } => TypeStructure::Opaque,
            SerialRustType::Reference(base_type) => TypeStructure::Pointer {
                referenced: map_box(base_type, |base_type| self.resolve_type2(
                    base_type,
                ))
            },
            SerialRustType::Tuple(elements) => TypeStructure::Tuple {
                elements: elements.into_iter().map(|elem| self.resolve_type2(elem)).collect()
            },
            SerialRustType::Array { elem, length } => TypeStructure::Array {
                elem: map_box(elem, |elem| self.resolve_type2(elem)),
                length
            },
            SerialRustType::Slice(elem) => TypeStructure::Slice {
                elem: map_box(elem, |elem| self.resolve_type2(elem))
            }
        };
        // use or, because structure.infer_size is guaranteed to be a no-op if known_size has any change of being Some
        StructuralRustType {
            type_name,
            size: known_size.or(structure.infer_size()),
            align: known_align.or(structure.infer_align()),
            structure
        }
    }

    fn infer_type_structurally(
        &mut self,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> StructuralRustType {
        match value {
            None => self.infer_type_structurally2(value_children),
            Some(SerialValueHead::Integer(_)) => StructuralRustType::i64(),
            Some(SerialValueHead::Float(_)) => StructuralRustType::f64(),
            Some(SerialValueHead::String(_)) => StructuralRustType::string(),
            Some(SerialValueHead::Ref { node_name: node_ident, field_name: field_ident }) => match self.resolved_node_type(node_ident) {
                // Error will show up later
                None => StructuralRustType::unknown(),
                Some(node_type) => match node_type.outputs.iter().find(|io_type| &io_type.name == field_ident) {
                    // Error will show up later
                    None => StructuralRustType::unknown(),
                    Some(io_type) => io_type.rust_type.clone_structural()
                }
            },
            Some(SerialValueHead::Tuple(elements)) => {
                let elements = elements.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let size = calculate_size(&elements);
                let align = calculate_align(&elements);
                let structure = TypeStructure::Tuple {
                    elements: elements.into_iter().map(RustType::Structural).collect()
                };
                StructuralRustType {
                    type_name: "{anonymous tuple}".to_string(),
                    size,
                    align,
                    structure
                }
            }
            Some(SerialValueHead::Array(elements)) => {
                let elements = elements.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let num_elements = elements.len();
                let element_type = self.merge_resolved_types(elements);
                let size = calculate_array_size(&element_type, num_elements);
                let align = element_type.align;
                let structure = TypeStructure::Array {
                    elem: Box::new(RustType::Structural(element_type)),
                    length: num_elements
                };
                StructuralRustType {
                    type_name: "{anonymous array}".to_string(),
                    size,
                    align,
                    structure
                }
            }
        }
    }

    fn infer_type_structurally2(
        &mut self,
        value_children: &SerialBody
    ) -> StructuralRustType {
        match value_children {
            SerialBody::None => StructuralRustType::unknown(),
            SerialBody::Tuple(tuple_items) => {
                let item_types = tuple_items.iter().map(|tuple_item| {
                    // TODO: why do we have to clone rust_type here? are we doing something redundant?
                    self.resolve_type(tuple_item.rust_type.clone(), (tuple_item.value.as_ref(), &tuple_item.value_children))
                }).collect::<Vec<_>>();
                let size = infer_tuple_size(&item_types);
                let align = infer_tuple_align(&item_types);
                let structure = TypeStructure::CReprStruct { body: TypeStructBody::Tuple(item_types) };
                StructuralRustType {
                    type_name: "{anonymous tuple struct}".to_string(),
                    size,
                    align,
                    structure
                }
            },
            SerialBody::Fields(fields) => {
                let item_types = fields.iter().map(|field| {
                    // TODO: why do we have to clone rust_type here? (see above)
                    self.resolve_type(field.rust_type.clone(), (field.value.as_ref(), &field.value_children))
                }).collect::<Vec<_>>();
                let size = infer_tuple_size(&item_types);
                let align = infer_tuple_align(&item_types);
                let structure = TypeStructure::CReprStruct {
                    body: TypeStructBody::Fields(
                        zip(
                            fields.iter().map(|field| &field.name).cloned(),
                            item_types.into_iter()
                        ).map(|(name, rust_type)| TypeStructField { name, rust_type }).collect::<Vec<_>>()
                    )
                };
                StructuralRustType {
                    type_name: "{anonymous field struct}".to_string(),
                    size,
                    align,
                    structure
                }
            }
        }
    }

    fn merge_resolved_type(
        &mut self,
        resolved_type: Option<StructuralRustType>,
        inferred_type: StructuralRustType
    ) -> StructuralRustType {
        match resolved_type {
            None => inferred_type,
            Some(mut resolved_type) => {
                if resolved_type.is_structural_subtype_of(&inferred_type) == IsSubtypeOf::No ||
                    inferred_type.is_structural_subtype_of(&resolved_type) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::ValueTypeMismatch {
                        explicit_type_name: resolved_type.type_name.to_string(),
                        inferred_type_name: inferred_type.type_name.to_string()
                    });
                }
                resolved_type.refine_from(inferred_type);
                resolved_type
            }
        }
    }

    fn merge_resolved_types(&mut self, resolved_types: Vec<StructuralRustType>) -> StructuralRustType {
        if resolved_types.is_empty() {
            return StructuralRustType::bottom();
        }

        let mut iter = resolved_types.into_iter();
        let mut final_type = iter.next().unwrap();
        for next_type in iter {
            if final_type.is_structural_subtype_of(&next_type) == IsSubtypeOf::No ||
                next_type.is_structural_subtype_of(&final_type) == IsSubtypeOf::No {
                self.errors.push(GraphFormError::ArrayElemTypeMismatch {
                    type_name_lhs: final_type.type_name.to_string(),
                    type_name_rhs: next_type.type_name.to_string()
                });
            }
            final_type.refine_from(next_type);
        }
        final_type
    }

    fn resolve_value(
        &mut self,
        (value, value_children): (Option<SerialValueHead>, SerialBody),
        rust_type: &RustType,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        match value {
            None => self.resolve_value_via_children(value_children, rust_type, node_name, field_name),
            Some(value) => {
                if !matches!(value_children, SerialBody::None) {
                    self.errors.push(GraphFormError::InlineValueHasChildren {
                        source: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                self.resolve_value_via_head(value, rust_type, node_name, field_name)
            }
        }
    }

    fn resolve_value_via_head(&mut self, value: SerialValueHead, rust_type: &RustType, node_name: &str, field_name: &str) -> NodeInput {
        match value {
            SerialValueHead::Integer(int) => NodeInput::Const(Box::new(int.to_ne_bytes())),
            SerialValueHead::Float(float) => NodeInput::Const(Box::new(float.to_ne_bytes())),
            SerialValueHead::String(string) => NodeInput::Const(string.into_boxed_str().into_boxed_bytes()),
            SerialValueHead::Ref { node_name: node_ident, field_name: field_ident } => {
                let (node_id, idx) = match self.resolved_node_and_type(&node_ident) {
                    None => {
                        self.errors.push(GraphFormError::NodeNotFound {
                            node_name: node_ident.to_string(),
                            referenced_from: NodeNameFieldName {
                                node_name: node_name.to_string(),
                                field_name: field_name.to_string()
                            }
                        });
                        (None, None)
                    },
                    Some((node_id, node_type, _)) => (
                        Some(*node_id),
                        match node_type.outputs.iter().position(|output| output.name == field_ident) {
                            None => {
                                self.errors.push(GraphFormError::FieldNotFound {
                                    field_name: field_ident.to_string(),
                                    node_name: node_ident.to_string(),
                                    referenced_from: NodeNameFieldName {
                                        node_name: node_name.to_string(),
                                        field_name: field_name.to_string()
                                    }
                                });
                                None
                            }
                            Some(idx) => Some(idx)
                        }
                    )
                };
                NodeInput::Dep(if node_id == Some(NodeId(usize::MAX)) {
                    NodeInputDep::GraphInput {
                        idx: idx.unwrap_or(usize::MAX)
                    }
                } else {
                    NodeInputDep::OtherNodeOutput {
                        id: node_id.unwrap_or(NodeId(usize::MAX)),
                        idx: idx.unwrap_or(usize::MAX)
                    }
                })
            }
            SerialValueHead::Array(elems) => {
                NodeInput::Array(elems.into_iter().map(|elem| {
                    self.resolve_value_via_head(elem, rust_type, node_name, field_name)
                }).collect())
            }
            SerialValueHead::Tuple(elems) => {
                NodeInput::Tuple(elems.into_iter().enumerate().map(|(index, elem)| {
                    self.resolve_value_child(index, (Some(elem), SerialBody::None), (rust_type, None), node_name, field_name)
                }).collect())
            }
        }
    }

    fn resolve_value_via_children(&mut self, value: SerialBody, rust_type: &RustType, node_name: &str, field_name: &str) -> NodeInput {
        match value {
            SerialBody::None => NodeInput::Hole,
            SerialBody::Tuple(tuple_items) => {
                NodeInput::Tuple(tuple_items.into_iter().enumerate().map(|(index, tuple_item)| {
                    self.resolve_value_child(index, (tuple_item.value, tuple_item.value_children), (rust_type, tuple_item.rust_type), node_name, field_name)
                }).collect())
            }
            SerialBody::Fields(fields) => {
                // field names are ignored in the input, and the data is equivalent to a tuple or unnamed struct
                NodeInput::Tuple(fields.into_iter().enumerate().map(|(index, field)| {
                    self.resolve_value_child(index, (field.value, field.value_children), (rust_type, field.rust_type), node_name, field_name)
                }).collect())
            }
        }
    }

    fn resolve_value_child(
        &mut self,
        index: usize,
        (elem, elem_children): (Option<SerialValueHead>, SerialBody),
        (rust_type, explicit_elem_type): (&RustType, Option<SerialRustType>),
        node_name: &str,
        field_name: &str
    ) -> NodeInputWithLayout {
        let explicit_elem_type = explicit_elem_type.map(|explicit_elem_type| {
            self.resolve_type2(explicit_elem_type)
        });
        let inferred_type_by_field_type = match rust_type.structure() {
            TypeStructure::Tuple { elements } => elements.get(index).cloned(),
            _ => None
        };
        let inferred_type_by_elem = RustType::Structural(self.infer_type_structurally((elem.as_ref(), &elem_children)));
        let inferred_type_without_explicit = match inferred_type_by_field_type {
            None => inferred_type_by_elem,
            Some(mut inferred_type_by_field_type) => {
                inferred_type_by_field_type.refine_from(inferred_type_by_elem);
                inferred_type_by_field_type
            }
        };
        let inferred_type = match explicit_elem_type {
            None => inferred_type_without_explicit,
            Some(mut explicit_type) => {
                explicit_type.refine_from(inferred_type_without_explicit);
                explicit_type
            }
        };
        let inferred_size = inferred_type.infer_size();
        let inferred_align = inferred_type.infer_align();

        if inferred_size.is_none() || inferred_align.is_none() {
            self.errors.push(GraphFormError::TupleElemLayoutNotResolved {
                inferred_type: inferred_type.to_string(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        }
        let size = inferred_size.unwrap_or(0);
        let align = inferred_align.unwrap_or(0);
        let input = self.resolve_value((elem, elem_children), rust_type, node_name, field_name);
        NodeInputWithLayout { input, size, align }
    }

    fn resolved_node_type(&self, node_name: &str) -> Option<&NodeTypeData> {
        self.resolved_nodes.get(node_name).map(|(_, node)| {
            self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type")
        })
    }

    fn resolved_node_and_type(&self, node_name: &str) -> Option<(&NodeId, &NodeTypeData, &Node)> {
        self.resolved_nodes.get(node_name).map(|(node_id, node)| {
            let node_type = self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type");
            (node_id, node_type, node)
        })
    }
}
