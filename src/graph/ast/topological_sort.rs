use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::{empty, once};

use crate::graph::StaticStrs;
use crate::graph::ast::types::{AstBody, AstEnumTypeDef, AstNode, AstRustType, AstStructTypeDef, AstTypeDef, AstTypeDefBody, AstValueHead};
//noinspection RsUnusedImport (IntelliJ fails to detect use)
use crate::graph::ast::types::AstFieldElem;
use crate::misc::extract::extract;

pub struct AstNodeDep<'a> {
    pub node_ident: &'a str,
    pub field_ident: &'a str
}

pub struct AstTypeDep<'a> {
    pub qualifiers: &'a Vec<String>,
    pub simple_name: &'a str
}

pub trait SortByDeps {
    type Elem: HasDeps;

    #[doc(hidden)]
    fn deps_map(&self) -> HashMap<String, HashSet<String>>;

    #[doc(hidden)]
    fn _sort_by(&mut self, compare: impl FnMut(&str, &str) -> Ordering);

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
            match (lhs_deps.contains(rhs_name), rhs_deps.contains(lhs_name)) {
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                // Fallback to comparing the name so that this is stable (useful e.g. for tests)
                (true, true) |
                (false, false) => lhs_name.cmp(rhs_name)
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

    fn _sort_by(&mut self, mut compare: impl FnMut(&str, &str) -> Ordering) {
        self.sort_by(|(lhs_name, _lhs), (rhs_name, _rhs)| {
            compare(lhs_name.as_str(), rhs_name.as_str())
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

    fn _sort_by(&mut self, mut compare: impl FnMut(&str, &str) -> Ordering) {
        self.sort_by(|(lhs_name, _lhs), (rhs_name, _rhs)| {
            compare(lhs_name.as_str(), rhs_name.as_str())
        });
    }
}

impl<'a, T: HasDeps> SortByDeps for [(&'a str, &'a mut T)] {
    type Elem = T;

    fn deps_map(&self) -> HashMap<String, HashSet<String>> {
        self.iter()
            .map(|(name, elem)| (name.to_string(), elem.deps_set()))
            .collect::<HashMap<_, _>>()
    }

    fn _sort_by(&mut self, mut compare: impl FnMut(&str, &str) -> Ordering) {
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

impl HasDeps for AstNode {
    fn deps_set(&self) -> HashSet<String> {
        node_dep_nodes(self).map(String::from).collect::<HashSet<_>>()
    }

    fn cmp_special_case(lhs_name: &str, rhs_name: &str) -> Ordering {
        // Handle if one of the nodes is an input or output.
        let input_case = (rhs_name == StaticStrs::INPUT_NODE).cmp(&(lhs_name == StaticStrs::INPUT_NODE));
        let output_case = (lhs_name == StaticStrs::OUTPUT_NODE).cmp(&(rhs_name == StaticStrs::OUTPUT_NODE));
        if input_case != Ordering::Equal {
            input_case
        } else if output_case != Ordering::Equal {
            output_case
        } else {
            Ordering::Equal
        }
    }
}

impl HasDeps for AstTypeDef {
    fn deps_set(&self) -> HashSet<String> {
        type_def_local_deps(self).map(String::from).collect::<HashSet<_>>()
    }

    fn cmp_special_case(_lhs_name: &str, _rhs_name: &str) -> Ordering {
        Ordering::Equal
    }
}

pub fn type_def_local_deps(type_def: &AstTypeDef) -> impl Iterator<Item=&str> {
    type_def_deps(type_def)
        .filter(|dep| dep.qualifiers.is_empty())
        .map(|dep| dep.simple_name)
}

pub fn type_def_deps(type_def: &AstTypeDef) -> impl Iterator<Item=AstTypeDep<'_>> {
    match type_def {
        AstTypeDef::Struct(struct_type) => Box::new(struct_type_def_deps(struct_type)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>,
        AstTypeDef::Enum(enum_type) => Box::new(enum_type_def_deps(enum_type)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
    }
}

fn struct_type_def_deps(struct_type: &AstStructTypeDef) -> impl Iterator<Item=AstTypeDep<'_>> {
    type_def_body_deps(&struct_type.body)
}

fn enum_type_def_deps(enum_type: &AstEnumTypeDef) -> impl Iterator<Item=AstTypeDep<'_>> {
    enum_type.variants.iter()
        .flat_map(|variant| type_def_body_deps(&variant.body))
}

fn type_def_body_deps(type_body: &AstTypeDefBody) -> impl Iterator<Item=AstTypeDep<'_>> {
    match type_body {
        AstTypeDefBody::None => Box::new(empty()),
        AstTypeDefBody::Tuple(tuple_items) => {
            Box::new(tuple_items.iter().flat_map(rust_type_deps)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstTypeDefBody::Fields(fields) => {
            Box::new(fields.iter().filter_map(|item| item.rust_type.as_ref()).flat_map(rust_type_deps)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
    }
}

fn rust_type_deps(rust_type: &AstRustType) -> impl Iterator<Item=AstTypeDep<'_>> {
    match rust_type {
        AstRustType::Ident { qualifiers, simple_name, generic_args: _ } => {
            // Generic args may add indirection, in which case we shouldn't count them.
            // Currently we assume all generic args are indirect, but in the future we may change this.
            // Either way generic args are only supported in registered builtin types.
            let dep = AstTypeDep { qualifiers, simple_name };
            Box::new(once(dep)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::ConstExpr { .. } => {
            Box::new(empty()) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::Anonymous { .. } => {
            Box::new(empty()) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::Pointer { .. } => {
            // Reference adds indirection, which we don't count as a dependency
            Box::new(empty()) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::Tuple { elems } => {
            Box::new(elems.iter().flat_map(rust_type_deps)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::Array { elem, length: _ } => {
            Box::new(rust_type_deps(elem)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
        AstRustType::Slice { elem } => {
            Box::new(rust_type_deps(elem)) as Box<dyn Iterator<Item=AstTypeDep<'_>>>
        }
    }
}

pub fn node_deps(node: &AstNode) -> impl Iterator<Item=AstNodeDep<'_>> {
    node.input_fields.iter()
        .filter_map(|field| extract!(field, AstFieldElem::Field { field }))
        .flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))
}

fn node_dep_nodes(node: &AstNode) -> impl Iterator<Item=&str> {
    node_deps(node).map(|dep| dep.node_ident)
}

fn value_deps<'a>(value: Option<&'a AstValueHead>, value_children: &'a AstBody) -> impl Iterator<Item=AstNodeDep<'a>> {
    value_head_deps(value).chain(value_children_deps(value_children))
}

fn value_head_deps(value_head: Option<&AstValueHead>) -> impl Iterator<Item=AstNodeDep> {
    match value_head {
        None | Some(AstValueHead::Literal(_)) => {
            Box::new(empty()) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        Some(AstValueHead::Ref { node_name: node_ident, field_name: field_ident }) => {
            Box::new(once(AstNodeDep { node_ident, field_ident })) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        Some(AstValueHead::Array(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        Some(AstValueHead::Tuple(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        Some(AstValueHead::Struct { type_name: _, inline_params }) |
        Some(AstValueHead::Enum { type_name: _, variant_name: _, inline_params }) => match inline_params {
            None => Box::new(empty()) as Box<dyn Iterator<Item=AstNodeDep>>,
            Some(inline_params) => {
                Box::new(inline_params.iter().flat_map(|inline_param| value_head_deps(Some(inline_param)))) as Box<dyn Iterator<Item=AstNodeDep>>
            }
        }
    }
}

fn value_children_deps(value_children: &AstBody) -> impl Iterator<Item=AstNodeDep> {
    match value_children {
        AstBody::None => {
            Box::new(empty()) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        AstBody::Tuple(elems) => {
            Box::new(elems.iter().flat_map(|elem| value_deps(elem.value.as_ref(), &elem.value_children))) as Box<dyn Iterator<Item=AstNodeDep>>
        },
        AstBody::Fields(fields) => {
            Box::new(fields.iter().flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))) as Box<dyn Iterator<Item=AstNodeDep>>
        }
    }
}