use std::fmt::{Display, Formatter};

use join_lazy_fmt::Join;
use snailquote::escape;

use crate::graph::ast::topological_sort::SortByDeps;
use crate::misc::fmt_with_ctx::{DisplayWithCtx, DisplayWithCtx2, Indent};
use crate::ast::types::{AstValueBody, AstField, AstFieldElem, AstFieldHeader, AstFieldTypeDef, AstGraph, AstLiteral, AstNode, AstTupleItem, AstTypeDef, AstTypeDefBody, AstValueHead, AstNodeAttr};
use structural_reflection::DuplicateNamesInScope;
use crate::StaticStrs;

impl Display for AstGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let dnis = self.iter_rust_types()
            .flat_map(|rust_type| rust_type.iter_simple_names())
            .collect::<DuplicateNamesInScope>();

        let mut rust_types = self.type_defs.iter().collect::<Vec<_>>();
        rust_types.sort_by_deps();
        let mut nodes = self.nodes.iter().collect::<Vec<_>>();
        nodes.sort_by_deps();

        for (type_name, rust_type) in rust_types {
            writeln!(f, "{}", rust_type.with_ctx((&dnis, type_name)))?;
        }

        for (node_name, node) in nodes {
            writeln!(f, "{}", node.with_ctx((&dnis, node_name)))?;
        }

        Ok(())
    }
}

impl DisplayWithCtx2 for AstTypeDef {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, type_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            AstTypeDef::Struct(struct_type) => {
                writeln!(f, "struct {} {{", type_name)?;
                writeln!(f, "{}", struct_type.body.with_ctx((dnis, &Indent(1))))?;
                writeln!(f, "}}")
            }
            AstTypeDef::Enum(enum_type) => {
                writeln!(f, "pub enum {} {{", type_name)?;
                for variant in &enum_type.variants {
                    writeln!(f, "  {}", variant.name)?;
                    writeln!(f, "{}", variant.body.with_ctx((dnis, &Indent(2))))?;
                }
                writeln!(f, "}}")
            }
        }
    }
}

impl DisplayWithCtx2 for AstTypeDefBody {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            AstTypeDefBody::None => {},
            AstTypeDefBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item.display(dnis))?;
                }
            }
            AstTypeDefBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field.with_ctx(dnis))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx for AstFieldTypeDef {
    type Ctx = DuplicateNamesInScope;

    fn fmt(&self, f: &mut Formatter<'_>, dnis: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.rust_type_may_be_null {
            write!(f, "?")?;
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(dnis))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for AstNode {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, node_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", node_name)?;
        if let Some(node_type) = self.node_type.as_ref() {
            write!(f, ": {}", node_type)?;
        }
        writeln!(f, "")?;

        for input_field in &self.input_fields {
            writeln!(f, "  {}", input_field.with_ctx((dnis, &Indent(1))))?;
        }
        if !self.output_fields.is_empty() {
            writeln!(f, "  ===")?;
            for output_field in &self.output_fields {
                writeln!(f, "  {}", output_field.with_ctx((dnis, &Indent(1))))?;
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for AstFieldElem {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            AstFieldElem::Header { header } => write!(f, "-- {}", header),
            AstFieldElem::Field { field } => write!(f, "{}", field.with_ctx((dnis, indent)))
        }
    }
}

impl Display for AstFieldHeader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstFieldHeader::Message(message) => write!(f, "{}", message),
            AstFieldHeader::NodeAttr(node_attr) => write!(f, "@{}", node_attr)
        }
    }
}

impl Display for AstNodeAttr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNodeAttr::Pos(pos) => write!(f, "pos {},{}", pos.x, pos.y),
            AstNodeAttr::PrimaryColor(color) => write!(f, "color lch,{},{},{}", color.l, color.c, color.h),
        }
    }
}

impl DisplayWithCtx2 for AstField {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.rust_type_may_be_null {
            write!(f, "?")?;
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(dnis))?;
        }
        if let Some(value) = self.value_head.as_ref() {
            write!(f, " = {}", value.with_ctx(dnis))?;
        }
        write!(f, "{}", self.value_children.with_ctx((dnis, &indent.next())))
    }
}

impl DisplayWithCtx2 for AstValueBody {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            AstValueBody::None => {},
            AstValueBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    write!(f, "\n{}{}", indent, tuple_item.with_ctx((dnis, &indent.next())))?;
                }
            }
            AstValueBody::Fields(fields) => {
                for field in fields {
                    write!(f, "\n{}{}", indent, field.with_ctx((dnis, &indent.next())))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for AstTupleItem {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (dnis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self.value.as_ref() {
            None => write!(f, "_")?,
            Some(value) => write!(f, "{}", value.with_ctx(dnis))?
        }
        if self.rust_type_may_be_null {
            write!(f, "?")?;
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(dnis))?;
        }
        write!(f, "{}", self.value_children.with_ctx((dnis, &indent.next())))
    }
}

impl DisplayWithCtx for AstValueHead {
    type Ctx = DuplicateNamesInScope;

    //noinspection DuplicatedCode
    fn fmt(&self, f: &mut Formatter<'_>, dnis: &Self::Ctx) -> std::fmt::Result {
        match self {
            AstValueHead::Literal(literal) => write!(f, "{}", literal),
            AstValueHead::Ref { node_name, field_name } => if field_name == StaticStrs::SELF_FIELD {
                write!(f, "{}", node_name)
            } else {
                write!(f, "{}.{}", node_name, field_name)
            },
            AstValueHead::InlineTuple(items) => write!(f, "({})", ", ".join(items.iter().map(|x| x.with_ctx(dnis)))),
            AstValueHead::InlineArray(elems) => write!(f, "[{}]", ", ".join(elems.iter().map(|x| x.with_ctx(dnis)))),
            AstValueHead::Struct { type_name: rust_type, inline_params } => {
                write!(f, "{}::", rust_type.display(dnis))?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(dnis))))?;
                }
                Ok(())
            },
            AstValueHead::Enum { type_name: rust_type, variant_name, inline_params } => {
                write!(f, "{}::{}", rust_type.display(dnis), variant_name)?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(dnis))))?;
                }
                Ok(())
            },
        }
    }
}

impl Display for AstLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstLiteral::Bool(value) => match value {
                false => write!(f, "false"),
                true => write!(f, "true")
            }
            AstLiteral::Integer(value) => write!(f, "{}", value),
            AstLiteral::Float(value) => write!(f, "{}", value),
            AstLiteral::String(value) => write!(f, "{}", escape(value)),
        }
    }
}
