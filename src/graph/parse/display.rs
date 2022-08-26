use std::fmt::{Display, Formatter};

use join_lazy_fmt::Join;
use snailquote::escape;

use crate::graph::parse::topological_sort::SortByDeps;
use crate::misc::fmt_with_ctx::{DisplayWithCtx, DisplayWithCtx2, Indent};
use crate::parse::types::{SerialBody, SerialField, SerialFieldElem, SerialFieldHeader, SerialFieldTypeDef, SerialGraph, SerialNode, SerialTupleItem, SerialTypeDef, SerialTypeDefBody, SerialValueHead};
use crate::rust_type::DuplicateNamesInScope;
use crate::StaticStrs;

impl Display for SerialGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let snis = self.iter_rust_types()
            .flat_map(|rust_type| rust_type.iter_snis())
            .collect::<DuplicateNamesInScope>();

        let mut rust_types = self.rust_types.iter().collect::<Vec<_>>();
        rust_types.sort_by_deps();
        let mut nodes = self.nodes.iter().collect::<Vec<_>>();
        nodes.sort_by_deps();

        for (type_name, rust_type) in rust_types {
            writeln!(f, "{}", rust_type.with_ctx((&snis, type_name)))?;
        }

        for (node_name, node) in nodes {
            writeln!(f, "{}", node.with_ctx((&snis, node_name)))?;
        }

        Ok(())
    }
}

impl DisplayWithCtx2 for SerialTypeDef {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, type_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialTypeDef::Struct(struct_type) => {
                writeln!(f, "struct {} {{", type_name)?;
                writeln!(f, "{}", struct_type.body.with_ctx((snis, &Indent(1))))?;
                writeln!(f, "}}")
            }
            SerialTypeDef::Enum(enum_type) => {
                writeln!(f, "pub enum {} {{", type_name)?;
                for variant in &enum_type.variants {
                    writeln!(f, "  {}", variant.name)?;
                    writeln!(f, "{}", variant.body.with_ctx((snis, &Indent(2))))?;
                }
                writeln!(f, "}}")
            }
        }
    }
}

impl DisplayWithCtx2 for SerialTypeDefBody {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialTypeDefBody::None => {},
            SerialTypeDefBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    writeln!(f, "{}{}", indent, tuple_item.display(snis))?;
                }
            }
            SerialTypeDefBody::Fields(fields) => {
                for field in fields {
                    writeln!(f, "{}{}", indent, field.with_ctx(snis))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx for SerialFieldTypeDef {
    type Ctx = DuplicateNamesInScope;

    fn fmt(&self, f: &mut Formatter<'_>, snis: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialNode {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = String;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, node_name): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", node_name)?;
        if let Some(node_type) = self.node_type.as_ref() {
            write!(f, ": {}", node_type)?;
        }
        writeln!(f, "")?;

        for input_field in &self.input_fields {
            writeln!(f, "  {}", input_field.with_ctx((snis, &Indent(1))))?;
        }
        if !self.output_fields.is_empty() {
            writeln!(f, "  ===")?;
            for output_field in &self.output_fields {
                writeln!(f, "  {}", output_field.with_ctx((snis, &Indent(1))))?;
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialFieldElem {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialFieldElem::Header { header } => write!(f, "-- {}", header),
            SerialFieldElem::Field { field } => write!(f, "{}", field.with_ctx((snis, indent)))
        }
    }
}

impl Display for SerialFieldHeader {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SerialFieldHeader::Message(message) => write!(f, "{}", message),
            SerialFieldHeader::Pos(pos) => write!(f, "@pos {},{}", pos.x, pos.y)
        }
    }
}

impl DisplayWithCtx2 for SerialField {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        if let Some(value) = self.value.as_ref() {
            write!(f, " = {}", value.with_ctx(snis))?;
        }
        write!(f, "{}", self.value_children.with_ctx((snis, &indent.next())))
    }
}

impl DisplayWithCtx2 for SerialBody {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self {
            SerialBody::None => {},
            SerialBody::Tuple(tuple_items) => {
                for tuple_item in tuple_items {
                    write!(f, "\n{}{}", indent, tuple_item.with_ctx((snis, &indent.next())))?;
                }
            }
            SerialBody::Fields(fields) => {
                for field in fields {
                    write!(f, "\n{}{}", indent, field.with_ctx((snis, &indent.next())))?;
                }
            }
        }
        Ok(())
    }
}

impl DisplayWithCtx2 for SerialTupleItem {
    type Ctx1 = DuplicateNamesInScope;
    type Ctx2 = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, (snis, indent): (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result {
        match self.value.as_ref() {
            None => write!(f, "_")?,
            Some(value) => write!(f, "{}", value.with_ctx(snis))?
        }
        if let Some(rust_type) = self.rust_type.as_ref() {
            write!(f, ": {}", rust_type.display(snis))?;
        }
        write!(f, "{}", self.value_children.with_ctx((snis, &indent.next())))
    }
}

impl DisplayWithCtx for SerialValueHead {
    type Ctx = DuplicateNamesInScope;

    //noinspection DuplicatedCode
    fn fmt(&self, f: &mut Formatter<'_>, snis: &Self::Ctx) -> std::fmt::Result {
        match self {
            SerialValueHead::Integer(value) => write!(f, "{}", value),
            SerialValueHead::Float(value) => write!(f, "{}", value),
            SerialValueHead::String(value) => write!(f, "{}", escape(value)),
            SerialValueHead::Ref { node_name, field_name } => if field_name == StaticStrs::SELF_FIELD {
                write!(f, "{}", node_name)
            } else {
                write!(f, "{}.{}", node_name, field_name)
            },
            SerialValueHead::Tuple(items) => write!(f, "({})", ", ".join(items.iter().map(|x| x.with_ctx(snis)))),
            SerialValueHead::Array(elems) => write!(f, "[{}]", ", ".join(elems.iter().map(|x| x.with_ctx(snis)))),
            SerialValueHead::Struct { type_name: rust_type, inline_params } => {
                write!(f, "{}", rust_type.display(snis))?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(snis))))?;
                }
                Ok(())
            },
            SerialValueHead::Enum { type_name: rust_type, variant_name, inline_params } => {
                write!(f, "{}::{}", rust_type.display(snis), variant_name)?;
                if let Some(inline_params) = inline_params.as_ref() {
                    write!(f, "({})", ", ".join(inline_params.iter().map(|x| x.with_ctx(snis))))?;
                }
                Ok(())
            },
        }
    }
}
