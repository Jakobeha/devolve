use std::iter::zip;
use crate::rust_type::{RustType, TypeStructBody};
use crate::rust_type::structure::{IsSubtypeOf, TypeStructure};

impl RustType {
    /// Returns true if both types have the same type id or name
    pub fn same(&self, other: &RustType) -> bool {
        if self.type_id.is_some() && other.type_id.is_some() {
            self.type_id == other.type_id
        } else {
            self.type_name == other.type_name
        }
    }

    /// Returns true if a value of this type can be casted to the other type,
    /// where the casting rules are as follows:
    ///
    /// - If both types have a type_id, then they must be equal
    /// - Otherwise, check that one type is a structural subtype of another.
    pub fn is_rough_subtype_of(&self, other: &RustType) -> IsSubtypeOf {
        if self.type_name.is_bottom() {
            IsSubtypeOf::Yes
        } else if other.type_name.is_bottom() {
            IsSubtypeOf::No
        } else if self.type_name.is_unknown() || other.type_name.is_unknown() {
            IsSubtypeOf::Unknown
        } else if self.type_id.is_some() && other.type_id.is_some() {
            IsSubtypeOf::known(self.type_id == other.type_id)
        } else {
            self.structure.is_structural_subtype_of(&other.structure)
        }
    }

    /// Attempts to unify with the other type, consuming it in the process:
    /// add more info if type type is opaque, and add more fields or less enum variants.
    ///
    /// This works like [TypeStructure::unify], except if this type has a type_id it will not
    /// change, and if this is unknown, it will become the other type.
    /// Also like [TypeStructure::unify], if these types are definitely different it will
    /// not change.
    pub fn unify(&mut self, other: RustType) {
        if self.type_name.is_unknown() {
            *self = other;
        } else if self.type_id.is_none() {
            self.structure.unify(other.structure);
        }
    }
}

impl TypeStructure {
    /// Returns true if a value of this type can be casted to the other type.
    /// Note that this may have more fields or less enum variants than the other type.
    pub fn is_structural_subtype_of(&self, other: &TypeStructure) -> IsSubtypeOf {
        match (self, other) {
            (TypeStructure::Opaque, _) | (_, TypeStructure::Opaque) => IsSubtypeOf::Unknown,
            (TypeStructure::Primitive(primitive), TypeStructure::Primitive(other_primitive)) => {
                IsSubtypeOf::known(primitive == other_primitive)
            },
            (TypeStructure::CReprEnum { variants }, TypeStructure::CReprEnum { variants: other_variants }) => {
                variants.iter().map(|variant| {
                    match other_variants.iter().find(|other_variant| variant.variant_name == other_variant.variant_name) {
                        None => IsSubtypeOf::No,
                        Some(other_variant) => variant.body.is_structural_subtype_of(&other_variant.body)
                    }
                }).min().unwrap_or(IsSubtypeOf::Yes)
            }
            (TypeStructure::CReprStruct { body }, TypeStructure::CReprStruct { body: other_body }) => {
                body.is_structural_subtype_of(other_body)
            }
            (TypeStructure::Pointer { referenced }, TypeStructure::Pointer { referenced: other_referenced }) => {
                IsSubtypeOf::known(referenced == other_referenced)
            }
            (TypeStructure::CTuple { elements }, TypeStructure::CTuple { elements: other_elements }) => {
                tuple_is_subtype_of(elements, other_elements)
            }
            (TypeStructure::Array { elem, length }, TypeStructure::Array { elem: other_elem, length: other_length }) => {
                if length != other_length {
                    IsSubtypeOf::No
                } else {
                    elem.is_rough_subtype_of(other_elem)
                }
            }
            (TypeStructure::Slice { elem }, TypeStructure::Slice { elem: other_elem }) => {
                elem.is_rough_subtype_of(other_elem)
            }
            _ => IsSubtypeOf::No
        }
    }

    /// Attempts to unify with the other type, consuming it in the process:
    /// add more info if type type is opaque, and add more fields or less enum variants.
    ///
    /// If these types are definitely different, than [unify] will not cause any changes.
    /// If you want different behavior (e.g. an error or bottom type), check
    /// [is_structural_subtype_of].
    pub fn unify(&mut self, other: TypeStructure) {
        // Can't put in match expr because of borrowing rules
        if matches!(self, TypeStructure::Opaque) {
            *self = other;
            return;
        }

        match (self, other) {
            (TypeStructure::Opaque, _) => unreachable!(),
            (TypeStructure::CReprEnum { variants }, TypeStructure::CReprEnum { variants: mut other_variants }) => {
                let _ = variants.drain_filter(|variant| {
                    if let Some(other_variant_idx) = other_variants.iter().position(|other_variant| variant.variant_name == other_variant.variant_name) {
                        let other_variant = other_variants.remove(other_variant_idx);
                        variant.body.unify(other_variant.body);
                        false
                    } else {
                        true
                    }
                });
            },
            (TypeStructure::CReprStruct { body }, TypeStructure::CReprStruct { body: other_body }) => {
                body.unify(other_body);
            }
            (TypeStructure::Pointer { referenced }, TypeStructure::Pointer { referenced: other_referenced }) => {
                referenced.unify(*other_referenced);
            }
            (TypeStructure::CTuple { elements }, TypeStructure::CTuple { elements: other_elements }) => {
                for (element, other_element) in elements.iter_mut().zip(other_elements.into_iter()) {
                    element.unify(other_element);
                }
            }
            (TypeStructure::Array { elem, length: _ }, TypeStructure::Array { elem: other_elem, length: _ }) => {
                elem.unify(*other_elem);
            }
            (TypeStructure::Slice { elem }, TypeStructure::Slice { elem: other_elem }) => {
                elem.unify(*other_elem);
            }
            _ => {}
        }
    }
}

impl TypeStructBody {
    fn is_structural_subtype_of(&self, other: &TypeStructBody) -> IsSubtypeOf {
        match (self, other) {
            (TypeStructBody::None, TypeStructBody::None) => IsSubtypeOf::Yes,
            (TypeStructBody::Tuple(elements), TypeStructBody::Tuple(other_elements)) => {
                tuple_is_subtype_of(elements, other_elements)
            }
            (TypeStructBody::Fields(fields), TypeStructBody::Fields(other_fields)) => {
                if fields.len() < other_fields.len() {
                    IsSubtypeOf::No
                } else {
                    other_fields.iter().map(|other_field| {
                        match fields.iter().find(|field| field.name == other_field.name) {
                            None => IsSubtypeOf::No,
                            Some(other_field) => field.rust_type.is_rough_subtype_of(&other_field.rust_type)
                        }
                    }).min().unwrap_or(IsSubtypeOf::Yes)
                }
            }
            _ => IsSubtypeOf::No
        }
    }

    fn unify(&mut self, other: TypeStructBody) {
        match (self, other) {
            (TypeStructBody::Tuple(elements), TypeStructBody::Tuple(other_elements)) => {
                for (element, other_element) in elements.iter_mut().zip(other_elements.into_iter()) {
                    element.unify(other_element);
                }
            }
            (TypeStructBody::Fields(fields), TypeStructBody::Fields(mut other_fields)) => {
                for field in fields.iter_mut() {
                    if let Some(other_field_idx) = other_fields.iter().position(|other_field| field.name == other_field.name) {
                        let other_field = other_fields.remove(other_field_idx);
                        field.rust_type.unify(other_field.rust_type);
                    }
                }
            }
            _ => {}
        }
    }
}

fn tuple_is_subtype_of(elements: &Vec<RustType>, other_elements: &Vec<RustType>) -> IsSubtypeOf {
    if elements.len() != other_elements.len() {
        IsSubtypeOf::No
    } else {
        zip(elements.iter(), other_elements.iter()).map(|(element, other_element)| {
            element.is_rough_subtype_of(other_element)
        }).min().unwrap_or(IsSubtypeOf::Yes)
    }
}

impl IsSubtypeOf {
    pub fn known(x: bool) -> Self {
        if x {
            IsSubtypeOf::Yes
        } else {
            IsSubtypeOf::No
        }
    }
}

