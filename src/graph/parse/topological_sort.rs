use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::{empty, once};

// Generally backwards dependencies like this (parse <- mutable) is bad.
// In this case we need NodeTypeName because we use mutable's dependency ordering
use crate::graph::mutable::NodeTypeName;
use crate::graph::parse::types::{SerialBody, SerialEnumTypeDef, SerialNode, SerialRustType, SerialStructTypeDef, SerialTypeDef, SerialTypeDefBody, SerialValueHead};
//noinspection RsUnusedImport (IntelliJ fails to detect use)
use crate::graph::parse::types::SerialFieldElem;
use crate::misc::extract::extract;

pub struct SerialNodeDep<'a> {
    pub node_ident: &'a str,
    pub field_ident: &'a str
}

pub struct SerialTypeDep<'a> {
    pub qualifiers: &'a Vec<String>,
    pub simple_name: &'a str
}

pub trait SortByDeps {
    type Elem: HasDeps;

    #[doc(hidden)]
    fn deps_map(&self) -> HashMap<String, HashSet<String>>;

    #[doc(hidden)]
    fn _sort_by(&mut self, compare: impl FnMut(&String, &String) -> Ordering);

    /// Note: When sorting nodes, the "Input" node is always first, the "Output" node is always last.
    /// This is true even if they have different dependency ordering
    /// (in this case, the dependency ordering is "wrong" -- we really just don't want to deal with this case)
    fn sort_by_deps(&mut self) {
        let deps_map = self.deps_map();

        self._sort_by(|lhs_name, rhs_name| {
            // Handle special case (for nodes)
            let special_case = Self::Elem::cmp_special_case(lhs_name, rhs_name);
            if special_case != Ordering::Equal {
                return special_case;
            }

            // Actually compare deps
            let lhs_deps = &deps_map[lhs_name];
            let rhs_deps = &deps_map[rhs_name];
            match (lhs_deps.contains(rhs_name.as_str()), rhs_deps.contains(lhs_name.as_str())) {
                // Fallback to comparing the name so that this is stable (useful e.g. for tests)
                (true, true) => lhs_name.cmp(rhs_name),
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => Ordering::Equal
            }
        });
    }
}

//noinspection DuplicatedCode
impl<T: HasDeps> SortByDeps for Vec<(String, T)> {
    type Elem = T;

    fn deps_map(&self) -> HashMap<String, HashSet<String>> {
        self.iter()
            .map(|(name, elem)| (name.to_string(), elem.deps_set()))
            .collect::<HashMap<_, _>>()
    }

    fn _sort_by(&mut self, mut compare: impl FnMut(&String, &String) -> Ordering) {
        self.sort_by(|(lhs_name, _lhs), (rhs_name, _rhs)| {
            compare(lhs_name, rhs_name)
        });
    }
}

//noinspection DuplicatedCode
impl<'a, T: HasDeps> SortByDeps for [(&'a String, &'a T)] {
    type Elem = T;

    fn deps_map(&self) -> HashMap<String, HashSet<String>> {
        self.iter()
            .map(|(name, elem)| (name.to_string(), elem.deps_set()))
            .collect::<HashMap<_, _>>()
    }

    fn _sort_by(&mut self, mut compare: impl FnMut(&String, &String) -> Ordering) {
        self.sort_by(|(lhs_name, _lhs), (rhs_name, _rhs)| {
            compare(lhs_name, rhs_name)
        });
    }
}

#[doc(hidden)]
pub trait HasDeps {
    fn deps_set(&self) -> HashSet<String>;
    fn cmp_special_case(lhs_name: &str, rhs_name: &str) -> Ordering;
}

impl HasDeps for SerialNode {
    fn deps_set(&self) -> HashSet<String> {
        node_dep_nodes(self).map(String::from).collect::<HashSet<_>>()
    }

    fn cmp_special_case(lhs_name: &str, rhs_name: &str) -> Ordering {
        // Handle if one of the nodes is an input or output.
        let input_case = (rhs_name == NodeTypeName::INPUT).cmp(&(lhs_name == NodeTypeName::INPUT));
        let output_case = (lhs_name == NodeTypeName::OUTPUT).cmp(&(rhs_name == NodeTypeName::OUTPUT));
        if input_case != Ordering::Equal {
            input_case
        } else if output_case != Ordering::Equal {
            output_case
        } else {
            Ordering::Equal
        }
    }
}

impl HasDeps for SerialTypeDef {
    fn deps_set(&self) -> HashSet<String> {
        type_def_local_deps(self).map(String::from).collect::<HashSet<_>>()
    }

    fn cmp_special_case(_lhs_name: &str, _rhs_name: &str) -> Ordering {
        Ordering::Equal
    }
}

pub fn type_def_local_deps(type_def: &SerialTypeDef) -> impl Iterator<Item=&str> {
    type_def_deps(type_def)
        .filter(|dep| dep.qualifiers.is_empty())
        .map(|dep| dep.simple_name)
}

pub fn type_def_deps(type_def: &SerialTypeDef) -> impl Iterator<Item=SerialTypeDep<'_>> {
    match type_def {
        SerialTypeDef::Struct(struct_type) => Box::new(struct_type_def_deps(struct_type)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>,
        SerialTypeDef::Enum(enum_type) => Box::new(enum_type_def_deps(enum_type)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
    }
}

fn struct_type_def_deps(struct_type: &SerialStructTypeDef) -> impl Iterator<Item=SerialTypeDep<'_>> {
    type_def_body_deps(&struct_type.body)
}

fn enum_type_def_deps(enum_type: &SerialEnumTypeDef) -> impl Iterator<Item=SerialTypeDep<'_>> {
    enum_type.variants.iter()
        .flat_map(|variant| type_def_body_deps(&variant.body))
}

fn type_def_body_deps(type_body: &SerialTypeDefBody) -> impl Iterator<Item=SerialTypeDep<'_>> {
    match type_body {
        SerialTypeDefBody::None => Box::new(empty()),
        SerialTypeDefBody::Tuple(tuple_items) => {
            Box::new(tuple_items.iter().flat_map(rust_type_deps)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialTypeDefBody::Fields(fields) => {
            Box::new(fields.iter().filter_map(|item| item.rust_type.as_ref()).flat_map(rust_type_deps)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
    }
}

fn rust_type_deps(rust_type: &SerialRustType) -> impl Iterator<Item=SerialTypeDep<'_>> {
    match rust_type {
        SerialRustType::Ident { qualifiers, simple_name, generic_args: _ } => {
            // Generic args may add indirection, in which case we shouldn't count them.
            // Currently we assume all generic args are indirect, but in the future we may change this.
            // Either way generic args are only supported in registered builtin types.
            let dep = SerialTypeDep { qualifiers, simple_name };
            Box::new(once(dep)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::ConstExpr { .. } => {
            Box::new(empty()) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::Anonymous { .. } => {
            Box::new(empty()) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::Pointer { .. } => {
            // Reference adds indirection, which we don't count as a dependency
            Box::new(empty()) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::Tuple { elems } => {
            Box::new(elems.iter().flat_map(rust_type_deps)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::Array { elem, length: _ } => {
            Box::new(rust_type_deps(elem)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
        SerialRustType::Slice { elem } => {
            Box::new(rust_type_deps(elem)) as Box<dyn Iterator<Item=SerialTypeDep<'_>>>
        }
    }
}

pub fn node_deps(node: &SerialNode) -> impl Iterator<Item=SerialNodeDep<'_>> {
    node.input_fields.iter()
        .filter_map(|field| extract!(field, SerialFieldElem::Field(field)))
        .flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))
}

fn node_dep_nodes(node: &SerialNode) -> impl Iterator<Item=&str> {
    node_deps(node).map(|dep| dep.node_ident)
}

fn value_deps<'a>(value: Option<&'a SerialValueHead>, value_children: &'a SerialBody) -> impl Iterator<Item=SerialNodeDep<'a>> {
    value_head_deps(value).chain(value_children_deps(value_children))
}

fn value_head_deps(value_head: Option<&SerialValueHead>) -> impl Iterator<Item=SerialNodeDep> {
    match value_head {
        None | Some(SerialValueHead::Integer(_)) | Some(SerialValueHead::Float(_)) | Some(SerialValueHead::String(_)) => {
            Box::new(empty()) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Ref { node_name: node_ident, field_name: field_ident }) => {
            Box::new(once(SerialNodeDep { node_ident, field_ident })) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Array(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Tuple(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Struct { type_name: _, inline_params }) |
        Some(SerialValueHead::Enum { type_name: _, variant_name: _, inline_params }) => match inline_params {
            None => Box::new(empty()) as Box<dyn Iterator<Item=SerialNodeDep>>,
            Some(inline_params) => {
                Box::new(inline_params.iter().flat_map(|inline_param| value_head_deps(Some(inline_param)))) as Box<dyn Iterator<Item=SerialNodeDep>>
            }
        }
    }
}

fn value_children_deps(value_children: &SerialBody) -> impl Iterator<Item=SerialNodeDep> {
    match value_children {
        SerialBody::None => {
            Box::new(empty()) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        SerialBody::Tuple(elems) => {
            Box::new(elems.iter().flat_map(|elem| value_deps(elem.value.as_ref(), &elem.value_children))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        SerialBody::Fields(fields) => {
            Box::new(fields.iter().flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))) as Box<dyn Iterator<Item=SerialNodeDep>>
        }
    }
}