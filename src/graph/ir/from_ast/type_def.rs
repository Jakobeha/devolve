use crate::error::GraphFormError;
use crate::ir::from_ast::GraphBuilder;
use crate::ast::types::{AstEnumTypeDef, AstEnumVariantTypeDef, AstFieldTypeDef, AstStructTypeDef, AstTypeDef, AstTypeDefBody, AstBody};
use structural_reflection::{RustType, RustTypeName, TypeEnumVariant, TypeStructureBody, TypeStructureBodyField, TypeStructure};

impl<'a> GraphBuilder<'a> {
    pub(super) fn resolve_type_def(&mut self, type_def_name: &str, type_def: AstTypeDef) -> RustType {
        match type_def {
            AstTypeDef::Struct(struct_type) => self.resolve_struct_type_def(type_def_name, struct_type),
            AstTypeDef::Enum(enum_type) => self.resolve_enum_type(type_def_name, enum_type)
        }
    }

    fn resolve_struct_type_def(&mut self, type_def_name: &str, struct_type_def: AstStructTypeDef) -> RustType {
        let structure = TypeStructure::CReprStruct {
            body: self.resolve_type_def_body(type_def_name, struct_type_def.body)
        };
        self.finish_resolving_type_def(type_def_name, structure)
    }

    fn resolve_enum_type(&mut self, type_def_name: &str, enum_type_def: AstEnumTypeDef) -> RustType {
        let structure = TypeStructure::CReprEnum {
            variants: enum_type_def.variants.into_iter().map(|variant| {
                self.resolve_variant_type_def(type_def_name, variant)
            }).collect()
        };
        self.finish_resolving_type_def(type_def_name, structure)
    }

    fn finish_resolving_type_def(&mut self, type_def_name: &str, structure: TypeStructure) -> RustType {
        let size = structure.infer_size();
        let align = structure.infer_align();
        if size.is_none() || align.is_none() {
            self.errors.push(GraphFormError::TypeDefLayoutNotResolved {
                type_def_name: type_def_name.to_string()
            });
        }
        RustType {
            type_id: None,
            type_name: RustTypeName::scoped_simple(self.ctx.qualifiers.clone(), type_def_name.to_string()),
            size: size.unwrap_or(usize::MAX),
            align: align.unwrap_or(usize::MAX),
            structure
        }
    }

    fn resolve_variant_type_def(&mut self, type_def_name: &str, variant_type: AstEnumVariantTypeDef) -> TypeEnumVariant {
        TypeEnumVariant {
            variant_name: variant_type.name,
            body: self.resolve_type_def_body(type_def_name, variant_type.body)
        }
    }

    fn resolve_type_def_body(&mut self, type_def_name: &str, type_def_body: AstTypeDefBody) -> TypeStructureBody {
        match type_def_body {
            AstTypeDefBody::None => TypeStructureBody::None,
            AstTypeDefBody::Tuple(elements) => TypeStructureBody::Tuple(
                elements.into_iter().map(|item| self.resolve_type2(item)).collect(),
            ),
            AstTypeDefBody::Fields(fields) => TypeStructureBody::Fields(
                fields.into_iter().map(|item| self.resolve_field_def(type_def_name, item)).collect(),
            )
        }
    }

    fn resolve_field_def(&mut self, type_def_name: &str, field_def: AstFieldTypeDef) -> TypeStructureBodyField {
        if field_def.default_value.is_some() || !matches!(field_def.default_value_children, AstBody::None) {
            self.errors.push(GraphFormError::FieldDefaultValueNotSupported {
                type_def_name: type_def_name.to_string(),
                field_name: field_def.name.to_string()
            });
        }
        TypeStructureBodyField {
            name: field_def.name,
            rust_type: self.resolve_type3(field_def.rust_type),
        }
    }
}