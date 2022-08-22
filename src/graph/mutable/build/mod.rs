use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::iter::zip;
use std::mem::take;

use slab::Slab;
use smallvec::SmallVec;

use crate::graph::builtins::{BuiltinNodeType, BuiltinNodeTypeFnCtx};
use crate::graph::error::{GraphFormError, GraphFormErrors, NodeNameFieldName};
use crate::graph::mutable::{FieldHeader, MutableGraph, Node, NodeId, NodeInput, NodeInputDep, NodeInputWithLayout, NodeIOType, NodeMetadata, NodeTypeData, NodeTypeName};
use crate::graph::parse::topological_sort::SortByDeps;
//noinspection RsUnusedImport (intelliJ fails to see SerialFieldElem use)
use crate::graph::parse::types::{SerialBody, SerialEnumType, SerialEnumVariantType, SerialField, SerialFieldElem, SerialFieldType, SerialGraph, SerialNode, SerialRustType, SerialStructType, SerialTypeDef, SerialTypeBody, SerialValueHead};
use crate::graph::raw::RawComputeFn;
use crate::rust_type::{infer_c_tuple_align, infer_c_tuple_size, RustType, RustTypeName, TypeEnumVariant, TypeStructure, TypeStructBody, TypeStructBodyForm, TypeStructField, IsSubtypeOf, IntrinsicRustType, PrimitiveType, infer_array_size, infer_array_align};

pub(super) struct GraphBuilder<'a> {
    qualifiers: Vec<String>,
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
    pub(super) fn build(graph: SerialGraph, module_qualifiers: Vec<String>, errors: &'a mut GraphFormErrors) -> MutableGraph {
        GraphBuilder {
            errors,
            qualifiers: module_qualifiers,
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

    fn resolve_type_def(&mut self, name: &str, type_def: SerialTypeDef) -> RustType {
        match type_def {
            SerialTypeDef::Struct(struct_type) => self.resolve_struct_type(name, struct_type),
            SerialTypeDef::Enum(enum_type) => self.resolve_enum_type(name, enum_type)
        }
    }

    fn resolve_struct_type(&mut self, name: &str, struct_type: SerialStructType) -> RustType {
        self.finish_resolving_type_def(name, TypeStructure::CReprStruct {
            body: self.resolve_type_body(struct_type.body)
        })
    }

    fn resolve_enum_type(&mut self, name: &str, enum_type: SerialEnumType) -> RustType {
        self.finish_resolving_type_def(name, TypeStructure::CReprEnum {
            variants: enum_type.variants.into_iter().map(|variant| self.resolve_variant_type(variant)).collect()
        })
    }

    fn finish_resolving_struct_type(&mut self, name: &str, structure: TypeStructure) -> RustType {
        let size = structure.infer_size();
        let align = structure.infer_align();
        if size.is_none() || align.is_none() {
            self.errors.push(GraphFormError::TypeDefLayoutNotResolved {
                name: name.to_string()
            });
        }
        RustType {
            type_id: None,
            type_name: RustTypeName::scoped_simple(self.qualifiers.clone(), name.to_string()),
            size: size.unwrap_or(usize::MAX),
            align: align.unwrap_or(usize::MAX),
            structure
        }
    }

    fn resolve_variant_type(&mut self, variant_type: SerialEnumVariantType) -> TypeEnumVariant {
        TypeEnumVariant {
            variant_name: variant_type.name,
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
        let value = self.resolve_value((field.value, field.value_children), Cow::Borrowed(&rust_type), node_name, &field.name);
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

    fn further_resolve_structural_type(&mut self, structural_type: RustType) -> RustType {
        match RustType::lookup(&structural_type.type_name) {
            None => structural_type,
            Some(known_type) => {
                if structural_type.structure.is_structural_subtype_of(&known_type.structure) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::TypeConflictsWithBuiltinType {
                        name: structural_type.type_name.to_string()
                    });
                }
                // Don't want to refine, although it probably won't do anything anyways:
                // we don't further refine known types with 'TypeId's.
                known_type
            }
        }
    }

    fn resolve_type_structurally(
        &mut self,
        serial_type: Option<SerialRustType>,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> RustType {
        let resolved_type = serial_type.map(|serial_type| self.resolve_type_structurally2(serial_type));
        let inferred_type = self.infer_type_structurally((value, value_children));
        self.merge_resolved_type(resolved_type, inferred_type)
    }

    fn resolve_type_structurally2(&mut self, serial_type: SerialRustType) -> RustType {
        let type_name = serial_type.to_string();
        let (known_size, known_align) = match &serial_type {
            SerialRustType::Ident { qualifiers, simple_name, generic_args } => {
                let (previously_defined_size, previously_defined_align) = if qualifiers == &self.qualifiers {
                    match self.resolved_rust_types.get(simple_name.as_str()) {
                        None => (None, None),
                        Some(previously_defined_type) => {
                            (previously_defined_type.size, previously_defined_type.align)
                        }
                    }
                } else {
                    None
                };
                let (intrinsic_size, intrinsic_align) = {
                    match RustType::lookup(&serial_type) {
                        None => (None, None),
                        Some(known_type) => (Some(known_type.size), Some(known_type.align))
                    }
                };
                // Support simple case of recursively-defined types:
                // we do this by registering a type with {unknown} on the params, which still has size and align
                let (generic_intrinsic_size, generic_intrinsic_align) = {
                    let mut serial_type_without_generics = serial_type.clone();
                    serial_type_without_generics.erase_generics();
                    match RustType::lookup(&serial_type_without_generics) {
                        None => (None, None),
                        Some(known_type) => (Some(known_type.size), Some(known_type.align))
                    }
                };
                (
                    intrinsic_size.or(generic_intrinsic_size).or(previously_defined_size),
                    intrinsic_align.or(generic_intrinsic_align).or(previously_defined_align)
                )
            },
            _ => (None, None)
        };
        let structure = match &serial_type {
            // Even if we defined the structure elsewhere, don't need to include it here
            // too much work and not actually necessary because we infer the size and alignment separately
            SerialRustType::Ident { .. } => TypeStructure::Opaque,
            SerialRustType::ConstExpr { .. } => TypeStructure::Opaque,
            SerialRustType::Anonymous { .. } => TypeStructure::Opaque,
            SerialRustType::Pointer { ptr_kind: _, refd } => TypeStructure::Pointer {
                refd: refd.lookup_back_intrinsic().unwrap_or_else(|| {
                    self.errors.push(GraphFormError::PointerToUnregisteredType {
                        refd_type_name: refd.as_ref().clone()
                    });
                    IntrinsicRustType::unknown()
                })
            },
            SerialRustType::Tuple(elements) => TypeStructure::CTuple {
                elements: elements.into_iter().map(|elem| self.resolve_type2(elem)).collect()
            },
            SerialRustType::Array { elem, length } => TypeStructure::Array {
                elem: Box::new(self.resolve_type2(elem.as_ref().clone())),
                length: *length
            },
            SerialRustType::Slice { elem } => TypeStructure::Slice {
                elem: Box::new(self.resolve_type2(elem.as_ref().clone()))
            },
        };
        // use or, because structure.infer_size is guaranteed to be a no-op if known_size has any chance of being Some
        let size = known_size.or(structure.infer_size());
        let align = known_align.or(structure.infer_align());
        if size.is_none() || align.is_none() {
            self.errors.push(GraphFormError::TypeLayoutNotResolved {
                type_name: serial_type.clone()
            });
        }
        RustType {
            type_id: None,
            type_name: serial_type,
            size: size.unwrap_or(usize::MAX),
            align: align.unwrap_or(usize::MAX),
            structure
        }
    }

    fn infer_type_structurally(
        &mut self,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> RustType {
        match value {
            None => self.infer_type_structurally2(value_children),
            Some(SerialValueHead::Integer(_)) => PrimitiveType::I64.rust_type(),
            Some(SerialValueHead::Float(_)) => PrimitiveType::F64.rust_type(),
            Some(SerialValueHead::String(_)) => RustType::lookup(&RustTypeName::simple(String::from("String"))).expect("String type should be defined"),
            Some(SerialValueHead::Ref { node_name: refd_node_name, field_name: refd_field_name }) => match self.resolved_node_type(refd_node_name) {
                // Error will show up later
                None => RustType::unknown(),
                Some(node_type) => match node_type.outputs.iter().find(|io_type| &io_type.name == refd_field_name) {
                    // Error will show up later
                    None => RustType::unknown(),
                    Some(io_type) => io_type.rust_type.clone_structural()
                }
            },
            Some(SerialValueHead::Tuple(elements)) => {
                let elements = elements.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let size = infer_c_tuple_size(&elements);
                let align = infer_c_tuple_align(&elements);
                let structure = TypeStructure::CTuple {
                    elements: elements.into_iter().map(RustType::Structural).collect()
                };
                RustType {
                    type_id: None,
                    type_name: RustTypeName::unknown(),
                    size,
                    align,
                    structure
                }
            }
            Some(SerialValueHead::Array(elements)) => {
                let elements = elements.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let num_elements = elements.len();
                let element_type = self.merge_resolved_types(elements);
                let size = infer_array_size(&element_type, num_elements);
                let align = infer_array_align(&element_type);
                let structure = TypeStructure::Array {
                    elem: Box::new(RustType::Structural(element_type)),
                    length: num_elements
                };
                RustType {
                    type_id: None,
                    type_name: RustTypeName::unknown(),
                    size,
                    align,
                    structure
                }
            }
            Some(SerialValueHead::Struct { type_name, inline_params }) => {
                let (size, align, body) = self.infer_type_body_structurally(inline_params.as_deref(), value_children);
                RustType {
                    type_id: None,
                    type_name: type_name.clone(),
                    // Ik we can infer size and alignment here,
                    // because either the struct type is known or we have an error
                    size,
                    align,
                    structure: TypeStructure::CReprStruct { body }
                }
            }
            Some(SerialValueHead::Enum { type_name, variant_name, inline_params }) => {
                let (size, align, body) = self.infer_type_body_structurally(inline_params.as_deref(), value_children);
                RustType {
                    type_id: None,
                    type_name: type_name.clone(),
                    // ^ same as in struct
                    size,
                    align,
                    structure: TypeStructure::CReprEnum {
                        // We can only infer this one variant
                        variants: vec![TypeEnumVariant {
                            variant_name: variant_name.to_string(),
                            body
                        }]
                    }
                }
            }
        }
    }

    fn infer_type_structurally2(&mut self, value_children: &SerialBody) -> RustType {
        if matches!(value_children, SerialBody::None) {
            return RustType::unknown();
        }
        let (size, align, body) = self.infer_type_body_structurally2(value_children);
        RustType {
            type_id: None,
            type_name: RustTypeName::unknown(),
            // ^ same as in infer_type_structurally, either we have the type or an error
            size: size.unwrap_or(usize::MAX),
            align: align.unwrap_or(usize::MAX),
            structure: TypeStructure::CReprStruct { body }
        }
    }

    fn infer_type_body_structurally(
        &mut self,
        inline_params: Option<&[SerialValueHead]>,
        value_children: &SerialBody
    ) -> (usize, usize, TypeStructBody) {
        match inline_params {
            None => self.infer_type_body_structurally2(value_children),
            Some(inline_params) => {
                let elements = inline_params.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let size = infer_c_tuple_size(&elements);
                let align = infer_c_tuple_align(&elements);
                let body = TypeStructBody::Tuple(elements.into_iter().map(RustType::Structural).collect());
                (size, align, body)
            }
        }
    }

    fn infer_type_body_structurally2(
        &mut self,
        value_children: &SerialBody
    ) -> (usize, usize, TypeStructBody) {
        match value_children {
            SerialBody::None => (0, 0, TypeStructBody::None),
            SerialBody::Tuple(tuple_items) => {
                let item_types = tuple_items.iter().map(|tuple_item| {
                    // why do we have to clone rust_type here? are we doing something redundant?
                    self.resolve_type(tuple_item.rust_type.clone(), (tuple_item.value.as_ref(), &tuple_item.value_children))
                }).collect::<Vec<_>>();
                let size = infer_c_tuple_size(&item_types);
                let align = infer_c_tuple_align(&item_types);
                let body = TypeStructBody::Tuple(item_types);
                (size, align, body)
            },
            SerialBody::Fields(fields) => {
                let item_types = fields.iter().map(|field| {
                    // why do we have to clone rust_type here? (see above)
                    self.resolve_type(field.rust_type.clone(), (field.value.as_ref(), &field.value_children))
                }).collect::<Vec<_>>();
                let size = infer_c_tuple_size(&item_types);
                let align = infer_c_tuple_align(&item_types);
                let body = TypeStructBody::Fields(
                    zip(
                        fields.iter().map(|field| &field.name).cloned(),
                        item_types.into_iter()
                    ).map(|(name, rust_type)| TypeStructField { name, rust_type }).collect::<Vec<_>>()
                );
                (size, align, body)
            }
        }
    }

    fn merge_resolved_type(
        &mut self,
        resolved_type: Option<RustType>,
        inferred_type: RustType
    ) -> RustType {
        match resolved_type {
            None => inferred_type,
            Some(mut resolved_type) => {
                if resolved_type.is_structural_subtype_of(&inferred_type) == IsSubtypeOf::No ||
                    inferred_type.is_structural_subtype_of(&resolved_type) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::ValueTypeMismatch {
                        explicit_type_name: resolved_type.type_name.clone(),
                        inferred_type_name: inferred_type.type_name.clone()
                    });
                }
                resolved_type.unify(inferred_type);
                resolved_type
            }
        }
    }

    fn merge_resolved_types(&mut self, resolved_types: Vec<RustType>) -> RustType {
        if resolved_types.is_empty() {
            return RustType::bottom();
        }

        let mut iter = resolved_types.into_iter();
        let mut final_type = iter.next().unwrap();
        for next_type in iter {
            if final_type.is_structural_subtype_of(&next_type) == IsSubtypeOf::No ||
                next_type.is_structural_subtype_of(&final_type) == IsSubtypeOf::No {
                self.errors.push(GraphFormError::ArrayElemTypeMismatch {
                    type_name_lhs: final_type.type_name.clone(),
                    type_name_rhs: next_type.type_name.clone()
                });
            }
            final_type.unify(next_type);
        }
        final_type
    }

    fn resolve_value(
        &mut self,
        (value, value_children): (Option<SerialValueHead>, SerialBody),
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        match value {
            None => self.resolve_value_via_children(value_children, rust_type, node_name, field_name),
            Some(SerialValueHead::Struct { type_name, inline_params }) => {
                let body = self.resolve_serial_constructor_body(inline_params, value_children, node_name, field_name);
                NodeInput::Tuple(self.resolve_struct_constructor(type_name, body, rust_type, node_name, field_name))
            }
            Some(SerialValueHead::Enum { type_name, variant_name, inline_params }) => {
                let body = self.resolve_serial_constructor_body(inline_params, value_children, node_name, field_name);
                NodeInput::Tuple(self.resolve_enum_constructor(type_name, variant_name, body, rust_type, node_name, field_name))
            }
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

    fn resolve_value_via_head(
        &mut self,
        value: SerialValueHead,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        match value {
            SerialValueHead::Integer(int) => NodeInput::Const(Box::new(int.to_ne_bytes())),
            SerialValueHead::Float(float) => NodeInput::Const(Box::new(float.to_ne_bytes())),
            SerialValueHead::String(string) => NodeInput::Const(string.into_boxed_str().into_boxed_bytes()),
            SerialValueHead::Ref { node_name: refd_node_name, field_name: refd_field_name } => {
                self.resolve_ref(refd_node_name, refd_field_name, node_name, field_name)
            }
            SerialValueHead::Array(elems) => {
                self.resolve_array_via_head(elems, rust_type, node_name, field_name)
            }
            SerialValueHead::Tuple(elems) => {
                self.resolve_tuple_via_head(elems, rust_type, node_name, field_name)
            },
            SerialValueHead::Struct { .. } | SerialValueHead::Enum { .. } => unreachable!("struct and enum may have children")
        }
    }

    fn resolve_serial_constructor_body(
        &mut self,
        inline_params: Option<Vec<SerialValueHead>>,
        value_children: SerialBody,
        node_name: &str,
        field_name: &str
    ) -> SerialBodyOrInlineTuple {
        match inline_params {
            None => SerialBodyOrInlineTuple::SerialBody(value_children),
            Some(inline_params) => {
                if !matches!(value_children, SerialBody::None) {
                    self.errors.push(GraphFormError::InlineValueHasChildren {
                        source: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                SerialBodyOrInlineTuple::InlineTuple { items: inline_params }
            }
        }
    }

    fn resolve_struct_constructor(
        &mut self,
        type_name: RustTypeName,
        body: SerialBodyOrInlineTuple,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        let resolved_type = match type_name {
            RustTypeName::Ident { qualifiers, simple_name, generic_args }
            if qualifiers == &self.qualifiers && generic_args.is_empty() => self.resolved_rust_types.get(simple_name.as_str()),
            _ => None
        };
        match resolved_type {
            None => {
                self.errors.push(GraphFormError::RustTypeNotFoundFromStructConstructor {
                    type_name: type_name.clone(),
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                // Best guess
                if matches!(rust_type.structure, TypeStructure::CReprStruct { body: _ }) {
                    self.resolve_struct_constructor_with_type(rust_type.clone_structural(), body, node_name, field_name)
                } else {
                    self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
                }
            }
            Some(explicit_rust_type) => {
                if explicit_rust_type.structure.is_structural_subtype_of(&rust_type.structure) == IsSubtypeOf::No ||
                    rust_type.structure.is_structural_subtype_of(&explicit_rust_type.structure) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::NestedValueTypeMismatch {
                        inferred_type_name: rust_type.type_name.clone(),
                        explicit_type_name: explicit_rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                let mut final_rust_type = explicit_rust_type.clone();
                final_rust_type.structure.unify(rust_type.into_owned().structure);
                self.resolve_struct_constructor_with_type(final_rust_type, body, node_name, field_name)
            }
        }
    }

    fn resolve_enum_constructor(
        &mut self,
        type_name: RustTypeName,
        variant_name: String,
        body: SerialBodyOrInlineTuple,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        let resolved_type = match type_name {
            RustTypeName::Ident { qualifiers, simple_name, generic_args }
            if qualifiers == &self.qualifiers && generic_args.is_empty() => self.resolved_rust_types.get(simple_name.as_str()),
            _ => None
        };
        match resolved_type {
            None => {
                self.errors.push(GraphFormError::RustTypeNotFoundFromEnumVariantConstructor {
                    type_name: type_name.clone(),
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                // Best guess
                if matches!(rust_type.structure, TypeStructure::CReprEnum { variants: _ }) {
                    self.resolve_enum_constructor_with_type(rust_type.clone_structural(), variant_name, body, node_name, field_name)
                } else {
                    self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
                }
            }
            Some(explicit_rust_type) => {
                if explicit_rust_type.structure.is_structural_subtype_of(&rust_type.structure) == IsSubtypeOf::No ||
                    rust_type.structure.is_structural_subtype_of(&explicit_rust_type.structure) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::NestedValueTypeMismatch {
                        inferred_type_name: rust_type.type_name.clone(),
                        explicit_type_name: explicit_rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                let mut final_rust_type = explicit_rust_type.clone();
                final_rust_type.structure.unify(rust_type.into_owned().structure);
                self.resolve_enum_constructor_with_type(final_rust_type, variant_name, body, node_name, field_name)
            }
        }
    }

    fn fallback_resolve_constructor_infer_type(
        &mut self,
        body: SerialBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        self._fallback_resolve_constructor_infer_type(body, node_name, field_name)
            .into_iter()
            .map(|input| {
                // Yeah we won't infer size and align because we can't compile anyways
                NodeInputWithLayout {
                    size: usize::MAX,
                    align: usize::MAX,
                    input
                }
            })
            .collect()
    }

    fn _fallback_resolve_constructor_infer_type(
        &mut self,
        body: SerialBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInput> {
        match body {
            SerialBodyOrInlineTuple::InlineTuple { items } => {
                items.into_iter().map(|item| {
                    self.resolve_value_via_head(item, Cow::Owned(RustType::unknown()), node_name, field_name)
                }).collect()
            },
            SerialBodyOrInlineTuple::SerialBody(SerialBody::None) => Vec::new(),
            SerialBodyOrInlineTuple::SerialBody(SerialBody::Tuple(tuple_items)) => {
                tuple_items.into_iter().map(|tuple_item| {
                    // ??? if creating the rust type here is correct. Maybe this is why we clone in the other place...
                    let rust_type = self.resolve_type3(tuple_item.rust_type);
                    self.resolve_value(
                        (tuple_item.value, tuple_item.value_children),
                        Cow::Owned(rust_type),
                        node_name,
                        field_name
                    )
                }).collect()
            }
            SerialBodyOrInlineTuple::SerialBody(SerialBody::Fields(fields)) => {
                fields.into_iter().map(|field| {
                    // ??? if creating the rust type here is correct. Maybe this is why we clone in the other place...
                    let rust_type = self.resolve_type3(field.rust_type);
                    self.resolve_value(
                        (field.value, field.value_children),
                        Cow::Owned(rust_type),
                        node_name,
                        field_name
                    )
                }).collect()
            }
        }
    }

    fn resolve_struct_constructor_with_type(
        &mut self,
        rust_type: RustType,
        body: SerialBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        let type_name = rust_type.type_name;
        match rust_type.structure {
            TypeStructure::CReprStruct { body: type_body } => {
                self.resolve_constructor_with_type(type_name, type_body, body, node_name, field_name)
            },
            _ => {
                self.errors.push(GraphFormError::RustTypeNotStructFromConstructor {
                    // The one time we don't have to clone because we consume rust_type...
                    type_name,
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
            }
        }
    }

    fn resolve_enum_constructor_with_type(
        &mut self,
        rust_type: RustType,
        variant_name: String,
        body: SerialBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        let type_name = rust_type.type_name;
        match rust_type.structure {
            TypeStructure::CReprEnum { variants } => {
                match variants.into_iter().find(|variant| &variant.variant_name == &variant_name) {
                    None => {
                        self.errors.push(GraphFormError::EnumVariantNotFound {
                            type_name,
                            variant_name,
                            referenced_from: NodeNameFieldName {
                                node_name: node_name.to_string(),
                                field_name: field_name.to_string()
                            }
                        });
                        self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
                    }
                    Some(variant) => {
                        self.resolve_constructor_with_type(
                            type_name,
                            variant.body,
                            body,
                            node_name,
                            field_name
                        )
                    }
                }
            },
            _ => {
                self.errors.push(GraphFormError::RustTypeNotEnumFromConstructor {
                    // The one time we don't have to clone because we consume rust_type...
                    // (other because of not abstracting)
                    type_name,
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
            }
        }
    }

    fn resolve_constructor_with_type(
        &mut self,
        type_name: RustTypeName,
        type_body: TypeStructBody,
        body: SerialBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeInputWithLayout> {
        match (type_body, body) {
            (TypeStructBody::None, SerialBodyOrInlineTuple::SerialBody(SerialBody::None)) => Vec::new(),
            (TypeStructBody::Tuple(tuple_item_types), SerialBodyOrInlineTuple::SerialBody(SerialBody::Tuple(tuple_items))) => {
                if tuple_item_types.len() != tuple_items.len() {
                    self.errors.push(GraphFormError::TupleLengthMismatch {
                        actual_length: tuple_items.len(),
                        type_length: tuple_item_types.len(),
                        type_name: type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                zip(tuple_item_types.into_iter(), tuple_items.into_iter()).map(|(tuple_item_type, tuple_item)| {
                    self.resolve_value_child(
                        (tuple_item.value, tuple_item.value_children),
                        (Some(tuple_item_type), tuple_item.rust_type),
                        node_name,
                        field_name
                    )
                }).collect()
            }
            (TypeStructBody::Tuple(tuple_item_types), SerialBodyOrInlineTuple::InlineTuple { items }) => {
                if tuple_item_types.len() != items.len() {
                    self.errors.push(GraphFormError::TupleLengthMismatch {
                        actual_length: items.len(),
                        type_length: tuple_item_types.len(),
                        type_name: type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                zip(tuple_item_types.into_iter(), items.into_iter()).map(|(tuple_item_type, item)| {
                    self.resolve_value_child(
                        (Some(item), SerialBody::None),
                        (Some(tuple_item_type), None),
                        node_name,
                        field_name
                    )
                }).collect()
            }
            (TypeStructBody::Fields(field_types), SerialBodyOrInlineTuple::SerialBody(SerialBody::Fields(mut fields))) => {
                for field in &fields {
                    if !field_types.iter().any(|field_type| &field_type.name == &field.name) {
                        self.errors.push(GraphFormError::RustFieldNotFound {
                            field_name: field.name.to_string(),
                            type_name: type_name.clone(),
                            referenced_from: NodeNameFieldName {
                                node_name: node_name.to_string(),
                                field_name: field_name.to_string()
                            }
                        });
                    }
                }
                field_types.into_iter().map(|field_type| {
                    let fields = fields
                        .drain_filter(|field| &field.name == &field_type.name)
                        .collect::<SmallVec<[SerialField; 1]>>();
                    if fields.len() > 1 {
                        self.errors.push(GraphFormError::RustFieldMultipleOccurrences {
                            field_name: field_type.name.to_string(),
                            referenced_from: NodeNameFieldName {
                                node_name: node_name.to_string(),
                                field_name: field_name.to_string()
                            }
                        });
                    }
                    let field = fields.into_iter().next();
                    let field_type = field_type.rust_type;

                    match field {
                        None => NodeInputWithLayout {
                            // idk what to put for the default size.
                            // It shouldn't build with holes which aren't filled by default values anyways,
                            // so I don't think it matters, but usize::MAX may trigger a panic overflow
                            size: field_type.infer_size().unwrap_or(usize::MAX),
                            align: field_type.infer_align().unwrap_or(usize::MAX),
                            input: NodeInput::Hole
                        },
                        Some(field) => {
                            self.resolve_value_child(
                                (field.value, field.value_children),
                                (Some(field_type), field.rust_type),
                                node_name,
                                field_name
                            )
                        }
                    }
                }).collect()
            },
            (type_body, body) => {
                self.errors.push(GraphFormError::RustTypeConstructorBadForm {
                    expected_form: type_body.form(),
                    actual_form: body.form(),
                    type_name: type_name.clone(),
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
            }
        }
    }
    
    fn resolve_ref(
        &mut self,
        refd_node_name: String,
        refd_field_name: String,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        let (node_id, idx) = match self.resolved_node_and_type(&refd_node_name) {
            None => {
                self.errors.push(GraphFormError::NodeNotFound {
                    node_name: refd_node_name.to_string(),
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                (None, None)
            },
            Some((node_id, node_type, _)) => (
                Some(*node_id),
                match node_type.outputs.iter().position(|output| output.name == refd_field_name) {
                    None => {
                        self.errors.push(GraphFormError::NodeFieldNotFound {
                            field_name: refd_field_name.to_string(),
                            node_name: refd_node_name.to_string(),
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
    
    fn resolve_array_via_head(
        &mut self,
        elements: Vec<SerialValueHead>,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        let elem_type = match rust_type.array_elem_type_and_length() {
            None => {
                self.errors.push(GraphFormError::NotAnArray {
                    type_name: rust_type.type_name.clone(),
                    referenced_from: NodeNameFieldName {
                        node_name: node_name.to_string(),
                        field_name: field_name.to_string()
                    }
                });
                Cow::Owned(RustType::unknown())
            }
            Some((elem_type, length)) => {
                if length != elements.len() {
                    self.errors.push(GraphFormError::TupleLengthMismatch {
                        actual_length: elements.len(),
                        type_length: length,
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                Cow::Borrowed(elem_type)
            }
        };
        NodeInput::Array(elements.into_iter().map(|element| {
            self.resolve_value_via_head(element, elem_type.clone(), node_name, field_name)
        }).collect())
    }

    fn resolve_tuple_via_head(
        &mut self,
        tuple_elems: Vec<SerialValueHead>,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeInput {
        let elem_types = rust_type.tuple_elem_types();
        if elem_types.is_none() {
            self.errors.push(GraphFormError::NotATuple {
                type_name: rust_type.type_name.clone(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        } else if elem_types.unwrap().len() != tuple_elems.len() {
            self.errors.push(GraphFormError::TupleLengthMismatch {
                actual_length: tuple_elems.len(),
                type_length: elem_types.unwrap().len(),
                type_name: rust_type.type_name.clone(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        }
        NodeInput::Tuple(tuple_elems.into_iter().enumerate().map(|(index, elem)| {
            let elem_type = elem_types.map(|elem_types| elem_types[index].clone());
            self.resolve_value_child(
                (Some(elem), SerialBody::None), 
                (elem_type, None), 
                node_name, 
                field_name
            )
        }).collect())
    }

    fn resolve_value_via_children(&mut self, value: SerialBody, rust_type: Cow<'_, RustType>, node_name: &str, field_name: &str) -> NodeInput {
        match value {
            SerialBody::None => NodeInput::Hole,
            SerialBody::Tuple(tuple_items) => {
                let tuple_item_types = rust_type
                    .tuple_struct_tuple_item_types()
                    .or(rust_type.tuple_elem_types());
                if tuple_item_types.is_none() {
                    self.errors.push(GraphFormError::NotATupleOrTupleStruct {
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                } else if tuple_item_types.unwrap().len() != tuple_items.len() {
                    self.errors.push(GraphFormError::TupleOrTupleStructLengthMismatch {
                        actual_length: tuple_items.len(),
                        type_length: tuple_item_types.unwrap().len(),
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                NodeInput::Tuple(tuple_items.into_iter().enumerate().map(|(index, tuple_item)| {
                    let tuple_item_type = tuple_item_types.map(|tuple_item_types| tuple_item_types[index].clone());
                    self.resolve_value_child(
                         (tuple_item.value, tuple_item.value_children),
                         (tuple_item_type, tuple_item.rust_type),
                         node_name,
                         field_name
                    )
                }).collect())
            }
            SerialBody::Fields(fields) => {
                let field_types = rust_type.field_struct_field_types();
                if field_types.is_none() {
                    self.errors.push(GraphFormError::NotAFieldStruct {
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                NodeInput::Tuple(fields.into_iter().map(|field| {
                    let field_type = field_types.and_then(|field_types| {
                        match field_types.iter().find(|field_type| &field_type.name == &field.name) {
                            None => {
                                self.errors.push(GraphFormError::RustFieldNotFound {
                                    field_name: field.name.to_string(),
                                    type_name: rust_type.type_name.clone(),
                                    referenced_from: NodeNameFieldName {
                                        node_name: node_name.to_string(),
                                        field_name: field_name.to_string()
                                    }
                                });
                                None
                            },
                            Some(field_type) => Some(field_type.rust_type.clone())
                        }
                    });
                    self.resolve_value_child(
                        (field.value, field.value_children),
                        (field_type, field.rust_type),
                        node_name,
                        field_name
                    )
                }).collect())
            }
        }
    }

    fn resolve_value_child(
        &mut self,
        (elem, elem_children): (Option<SerialValueHead>, SerialBody),
        (inferred_elem_type, explicit_elem_type): (Option<RustType>, Option<SerialRustType>),
        node_name: &str,
        field_name: &str
    ) -> NodeInputWithLayout {
        let explicit_elem_type = explicit_elem_type.map(|explicit_elem_type| {
            self.resolve_type2(explicit_elem_type)
        });
        let rough_inferred_elem_type = RustType::Structural(self.infer_type_structurally((elem.as_ref(), &elem_children)));
        let inferred_elem_type = match inferred_elem_type {
            None => rough_inferred_elem_type,
            Some(mut provided_elem_type) => {
                provided_elem_type.unify(rough_inferred_elem_type);
                provided_elem_type
            }
        };
        let elem_type = match explicit_elem_type {
            None => inferred_elem_type,
            Some(mut explicit_elem_type) => {
                if inferred_elem_type.is_rough_subtype_of(&explicit_elem_type) == IsSubtypeOf::No ||
                    explicit_elem_type.is_rough_subtype_of(&inferred_elem_type) == IsSubtypeOf::No {
                    self.errors.push(GraphFormError::NestedValueTypeMismatch {
                        inferred_type_name: inferred_elem_type.type_name.clone(),
                        explicit_type_name: explicit_elem_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                explicit_elem_type.unify(inferred_elem_type);
                explicit_elem_type
            }
        };
        let inferred_size = elem_type.infer_size();
        let inferred_align = elem_type.infer_align();

        if inferred_size.is_none() || inferred_align.is_none() {
            self.errors.push(GraphFormError::ElemLayoutNotResolved {
                inferred_type: elem_type.to_string(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        }
        let size = inferred_size.unwrap_or(0);
        let align = inferred_align.unwrap_or(0);
        let input = self.resolve_value((elem, elem_children), Cow::Owned(elem_type), node_name, field_name);
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