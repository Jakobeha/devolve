use std::iter::zip;
use crate::error::GraphFormError;
use crate::mutable::build::GraphBuilder;
use crate::parse::types::{SerialBody, SerialRustType, SerialValueHead};
use crate::rust_type::{infer_array_align, infer_array_size, infer_c_tuple_align, infer_c_tuple_size, IntrinsicRustType, IsSubtypeOf, PrimitiveType, RustType, RustTypeName, TypeEnumVariant, TypeStructBody, TypeStructField, TypeStructure};

impl<'a> GraphBuilder<'a> {
    pub(super) fn resolve_type(
        &mut self,
        serial_type: Option<SerialRustType>,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody)
    ) -> RustType {
        let structural_type = self.resolve_type_structurally(serial_type, (value, value_children));
        self.further_resolve_structural_type(structural_type)
    }

    pub(super) fn resolve_type2(&mut self, serial_type: SerialRustType) -> RustType {
        let structural_type = self.resolve_type_structurally2(serial_type);
        self.further_resolve_structural_type(structural_type)
    }

    pub(super) fn resolve_type3(&mut self, serial_type: Option<SerialRustType>) -> RustType {
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
                    self.errors.push(GraphFormError::TypeConflictsWithRegisteredType {
                        type_name: structural_type.type_name.clone()
                    });
                }
                // Don't want to refine, although it shouldn't do anything anyways:
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
        let (known_size, known_align) = match &serial_type {
            SerialRustType::Ident { qualifiers, simple_name, generic_args: _ } => {
                let (previously_defined_size, previously_defined_align) = if qualifiers == &self.ctx.qualifiers {
                    match self.resolved_rust_types.get(simple_name.as_str()) {
                        None => (None, None),
                        Some(previously_defined_type) => {
                            (Some(previously_defined_type.size), Some(previously_defined_type.align))
                        }
                    }
                } else {
                    (None, None)
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
            SerialRustType::Tuple { elems: elements } => TypeStructure::CTuple {
                elements: elements.iter().map(|element| self.resolve_type2(element.clone())).collect()
            },
            SerialRustType::Array { elem, length } => TypeStructure::Array {
                elem: Box::new(self.resolve_type2(elem.as_ref().clone())),
                length: *length
            },
            // There is no Slice TypeStructure
            SerialRustType::Slice { .. } => TypeStructure::Opaque
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

    pub(super) fn infer_type_structurally(
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
                    Some(io_type) => io_type.rust_type.clone()
                }
            },
            Some(SerialValueHead::Tuple(elements)) => {
                let elements = elements.iter().map(|elem| self.infer_type_structurally((Some(elem), &SerialBody::None))).collect::<Vec<_>>();
                let size = infer_c_tuple_size(&elements);
                let align = infer_c_tuple_align(&elements);
                let structure = TypeStructure::CTuple { elements };
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
                    elem: Box::new(element_type),
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
            size,
            align,
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
                let body = TypeStructBody::Tuple(elements);
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
                if resolved_type.is_rough_subtype_of(&inferred_type) == IsSubtypeOf::No ||
                    inferred_type.is_rough_subtype_of(&resolved_type) == IsSubtypeOf::No {
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
            if final_type.is_rough_subtype_of(&next_type) == IsSubtypeOf::No ||
                next_type.is_rough_subtype_of(&final_type) == IsSubtypeOf::No {
                self.errors.push(GraphFormError::ArrayElemTypeMismatch {
                    type_name_lhs: final_type.type_name.clone(),
                    type_name_rhs: next_type.type_name.clone()
                });
            }
            final_type.unify(next_type);
        }
        final_type
    }
}