use std::borrow::Cow;
use std::iter::zip;
use smallvec::SmallVec;
use crate::error::{GraphFormError, NodeNameFieldName};
use crate::ir::from_ast::{GraphBuilder, AstBodyOrInlineTuple};
use crate::ir::{NodeId, NodeIO, NodeIODep, NodeIOWithLayout};
use crate::ast::types::{AstValueBody, AstField, AstLiteral, AstRustType, AstValueHead};
use structural_reflection::{IsSubtypeOf, RustType, RustTypeName, TypeStructureBody, TypeStructure};
use crate::misc::inline_ptr::InlinePtr;

impl<'a, RuntimeCtx: ?Sized> GraphBuilder<'a, RuntimeCtx> {
    pub(super) fn resolve_value(
        &mut self,
        (value, value_children): (Option<AstValueHead>, AstValueBody),
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeIO {
        match value {
            None => self.resolve_value_via_children(value_children, rust_type, node_name, field_name),
            Some(AstValueHead::Struct { type_name, inline_params }) => {
                let body = self.resolve_ast_constructor_body(inline_params, value_children, node_name, field_name);
                NodeIO::Tuple(self.resolve_struct_constructor(type_name, body, rust_type, node_name, field_name))
            }
            Some(AstValueHead::Enum { type_name, variant_name, inline_params }) => {
                let body = self.resolve_ast_constructor_body(inline_params, value_children, node_name, field_name);
                NodeIO::Tuple(self.resolve_enum_constructor(type_name, variant_name, body, rust_type, node_name, field_name))
            }
            Some(value) => {
                if !matches!(value_children, AstValueBody::None) {
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
        value: AstValueHead,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeIO {
        match value {
            AstValueHead::Literal(literal) => match literal {
                AstLiteral::Bool(bool) => NodeIO::ConstInline(Box::new([if bool { 1u8 } else { 0u8 }])),
                AstLiteral::Integer(int) => NodeIO::ConstInline(Box::new(int.to_ne_bytes())),
                AstLiteral::Float(float) => NodeIO::ConstInline(Box::new(float.to_ne_bytes())),
                // SAFETY: String pool is stored with the ir-graph and lower-graph so it is guaranteed to retain the string pointer
                AstLiteral::String(string) => {
                    let ptr = self.constant_pool.get_or_insert(string);
                    NodeIO::ConstRef(InlinePtr::new(ptr as *const str))
                }
            },
            AstValueHead::Ref { node_name: refd_node_name, field_name: refd_field_name } => {
                self.resolve_ref(refd_node_name, refd_field_name, node_name, field_name)
            }
            AstValueHead::InlineArray(elems) => {
                self.resolve_array_via_head(elems, rust_type, node_name, field_name)
            }
            AstValueHead::InlineTuple(elems) => {
                self.resolve_tuple_via_head(elems, rust_type, node_name, field_name)
            },
            AstValueHead::Struct { .. } | AstValueHead::Enum { .. } => unreachable!("struct and enum may have children")
        }
    }

    fn resolve_ast_constructor_body(
        &mut self,
        inline_params: Option<Vec<AstValueHead>>,
        value_children: AstValueBody,
        node_name: &str,
        field_name: &str
    ) -> AstBodyOrInlineTuple {
        match inline_params {
            None => AstBodyOrInlineTuple::AstBody(value_children),
            Some(inline_params) => {
                if !matches!(value_children, AstValueBody::None) {
                    self.errors.push(GraphFormError::InlineValueHasChildren {
                        source: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                AstBodyOrInlineTuple::InlineTuple { items: inline_params }
            }
        }
    }

    fn resolve_struct_constructor(
        &mut self,
        type_name: RustTypeName,
        body: AstBodyOrInlineTuple,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
        let resolved_type = match &type_name {
            RustTypeName::Ident { qualifier, simple_name, generic_args }
            if qualifier == &self.ctx.qualifier && generic_args.is_empty() => self.resolved_rust_types.get(simple_name.as_str()),
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
                    self.resolve_struct_constructor_with_type(rust_type.into_owned(), body, node_name, field_name)
                } else {
                    self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
                }
            }
            Some(explicit_rust_type) => {
                if rust_type.structure.is_structural_subtype_of(&explicit_rust_type.structure) == IsSubtypeOf::No {
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
        body: AstBodyOrInlineTuple,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
        let resolved_type = match &type_name {
            RustTypeName::Ident { qualifier, simple_name, generic_args }
            if qualifier == &self.ctx.qualifier && generic_args.is_empty() => self.resolved_rust_types.get(simple_name.as_str()),
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
                    self.resolve_enum_constructor_with_type(rust_type.into_owned(), variant_name, body, node_name, field_name)
                } else {
                    self.fallback_resolve_constructor_infer_type(body, node_name, field_name)
                }
            }
            Some(explicit_rust_type) => {
                if rust_type.structure.is_structural_subtype_of(&explicit_rust_type.structure) == IsSubtypeOf::No {
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
        body: AstBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
        self._fallback_resolve_constructor_infer_type(body, node_name, field_name)
            .into_iter()
            .map(|input| {
                // Yeah we won't infer size and align because we can't compile anyways
                NodeIOWithLayout {
                    size: usize::MAX,
                    align: usize::MAX,
                    input
                }
            })
            .collect()
    }

    fn _fallback_resolve_constructor_infer_type(
        &mut self,
        body: AstBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIO> {
        match body {
            AstBodyOrInlineTuple::InlineTuple { items } => {
                items.into_iter().map(|item| {
                    self.resolve_value_via_head(item, Cow::Owned(RustType::unknown()), node_name, field_name)
                }).collect()
            },
            AstBodyOrInlineTuple::AstBody(AstValueBody::None) => Vec::new(),
            AstBodyOrInlineTuple::AstBody(AstValueBody::Tuple(tuple_items)) => {
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
            AstBodyOrInlineTuple::AstBody(AstValueBody::Fields(fields)) => {
                fields.into_iter().map(|field| {
                    // ??? if creating the rust type here is correct. Maybe this is why we clone in the other place...
                    let rust_type = self.resolve_type3(field.rust_type);
                    self.resolve_value(
                        (field.value_head, field.value_children),
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
        body: AstBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
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
        body: AstBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
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
        type_body: TypeStructureBody,
        body: AstBodyOrInlineTuple,
        node_name: &str,
        field_name: &str
    ) -> Vec<NodeIOWithLayout> {
        match (type_body, body) {
            (TypeStructureBody::None, AstBodyOrInlineTuple::AstBody(AstValueBody::None)) => Vec::new(),
            (TypeStructureBody::Tuple(tuple_item_types), AstBodyOrInlineTuple::AstBody(AstValueBody::Tuple(tuple_items))) => {
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
            (TypeStructureBody::Tuple(tuple_item_types), AstBodyOrInlineTuple::InlineTuple { items }) => {
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
                        (Some(item), AstValueBody::None),
                        (Some(tuple_item_type), None),
                        node_name,
                        field_name
                    )
                }).collect()
            }
            (TypeStructureBody::Fields(field_types), AstBodyOrInlineTuple::AstBody(AstValueBody::Fields(mut fields))) => {
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
                        .collect::<SmallVec<[AstField; 1]>>();
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
                        None => NodeIOWithLayout {
                            size: field_type.size,
                            align: field_type.align,
                            input: NodeIO::Hole
                        },
                        Some(field) => {
                            self.resolve_value_child(
                                (field.value_head, field.value_children),
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
    ) -> NodeIO {
        let (node_id, field_idx) = match self.resolved_node_and_type(&refd_node_name) {
            None => match self.forward_resolved_node(&refd_node_name) {
                Some((node_id, forward_node)) => {
                    // Still try to resolve field name, so we can also report field-not-found
                    let field_idx = match &forward_node.input_field_names {
                        None => {
                            // Fill with some dummy value so we don't get NodeFieldNotFound error
                            Some(usize::MAX)
                        },
                        Some(input_field_names) => input_field_names.iter().position(|name| name == &refd_field_name)
                    };

                    self.errors.push(GraphFormError::CyclicReference {
                        node_name: refd_node_name.to_string(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });

                    (Some(node_id), field_idx)
                },
                None => (None, None)
            },
            Some((node_id, node_type, _)) => {
                (Some(node_id), node_type.outputs.iter().position(|field_type| &field_type.name == &refd_field_name))
            }
        };

        if node_id.is_none() {
            self.errors.push(GraphFormError::NodeNotFound {
                node_name: refd_node_name.to_string(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        }
        if field_idx.is_none() {
            self.errors.push(GraphFormError::NodeFieldNotFound {
                field_name: refd_field_name.to_string(),
                node_name: refd_node_name.to_string(),
                referenced_from: NodeNameFieldName {
                    node_name: node_name.to_string(),
                    field_name: field_name.to_string()
                }
            });
        }

        NodeIO::Dep(if node_id == Some(NodeId(usize::MAX)) {
            NodeIODep::GraphInput {
                idx: field_idx.unwrap_or(usize::MAX)
            }
        } else {
            NodeIODep::OtherNodeOutput {
                id: node_id.unwrap_or(NodeId(usize::MAX)),
                idx: field_idx.unwrap_or(usize::MAX)
            }
        })
    }

    fn resolve_array_via_head(
        &mut self,
        elements: Vec<AstValueHead>,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeIO {
        let elem_type = match rust_type.structure.array_elem_type_and_length() {
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
        NodeIO::Array(elements.into_iter().map(|element| {
            self.resolve_value_via_head(element, elem_type.clone(), node_name, field_name)
        }).collect())
    }

    fn resolve_tuple_via_head(
        &mut self,
        tuple_elems: Vec<AstValueHead>,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeIO {
        let elem_types = rust_type.structure.tuple_elem_types();
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
        NodeIO::Tuple(tuple_elems.into_iter().enumerate().map(|(index, elem)| {
            let elem_type = elem_types.map(|elem_types| elem_types[index].clone());
            self.resolve_value_child(
                (Some(elem), AstValueBody::None),
                (elem_type, None),
                node_name,
                field_name
            )
        }).collect())
    }

    fn resolve_value_via_children(
        &mut self,
        value: AstValueBody,
        rust_type: Cow<'_, RustType>,
        node_name: &str,
        field_name: &str
    ) -> NodeIO {
        match value {
            AstValueBody::None => NodeIO::Hole,
            AstValueBody::Tuple(tuple_items) => {
                let tuple_item_types = rust_type.structure
                    .general_tuple_item_types2(tuple_items.len())
                    .map(|tuple_item_types| tuple_item_types.collect::<Vec<_>>());
                if tuple_item_types.is_none() {
                    self.errors.push(GraphFormError::NotATupleOrTupleStruct {
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                } else if tuple_item_types.as_ref().unwrap().len() != tuple_items.len() {
                    self.errors.push(GraphFormError::TupleOrTupleStructLengthMismatch {
                        actual_length: tuple_items.len(),
                        type_length: tuple_item_types.as_ref().unwrap().len(),
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                NodeIO::Tuple(tuple_items.into_iter().enumerate().map(|(index, tuple_item)| {
                    let tuple_item_type = tuple_item_types.as_ref().map(|tuple_item_types| tuple_item_types[index].clone());
                    self.resolve_value_child(
                        (tuple_item.value, tuple_item.value_children),
                        (tuple_item_type, tuple_item.rust_type),
                        node_name,
                        field_name
                    )
                }).collect())
            }
            AstValueBody::Fields(fields) => {
                let field_types = rust_type.structure.field_struct_field_types();
                if field_types.is_none() {
                    self.errors.push(GraphFormError::NotAFieldStruct {
                        type_name: rust_type.type_name.clone(),
                        referenced_from: NodeNameFieldName {
                            node_name: node_name.to_string(),
                            field_name: field_name.to_string()
                        }
                    });
                }
                NodeIO::Tuple(fields.into_iter().map(|field| {
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
                        (field.value_head, field.value_children),
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
        (elem, elem_children): (Option<AstValueHead>, AstValueBody),
        (inferred_elem_type, explicit_elem_type): (Option<RustType>, Option<AstRustType>),
        node_name: &str,
        field_name: &str
    ) -> NodeIOWithLayout {
        let explicit_elem_type = explicit_elem_type.map(|explicit_elem_type| {
            self.resolve_type2(explicit_elem_type)
        });
        let rough_inferred_elem_type = self.infer_type_structurally((elem.as_ref(), &elem_children));
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
                if inferred_elem_type.is_rough_subtype_of(&explicit_elem_type) == IsSubtypeOf::No {
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
        let size = elem_type.size;
        let align = elem_type.align;
        let input = self.resolve_value((elem, elem_children), Cow::Owned(elem_type), node_name, field_name);
        NodeIOWithLayout { input, size, align }
    }

}