use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use iter_enum::Iterator;

//noinspection RsUnusedImport (intelliJ fails to detect SerialFieldElem use)
use crate::graph::parse::types::{SerialBody, SerialField, SerialFieldElem, SerialNode, SerialTupleItem, SerialValueHead};
use crate::misc::extract::extract;

struct SerialNodeDep<'a> {
    pub node_ident: &'a str,
    pub field_ident: &'a str
}

pub fn sort_nodes_by_deps(nodes: HashMap<String, SerialNode>) -> Vec<(String, SerialNode)> {
    let deps_map = nodes.iter()
        .map(|(node_name, node)| (node_name.to_string(), node_dep_nodes(node).collect::<HashSet<_>>()))
        .collect::<HashMap<_, _>>();
    let mut nodes = nodes.into_iter().collect::<Vec<_>>();

    nodes.sort_by(|(a_name, _), (b_name, _)| {
        let a_deps = &deps_map[a_name];
        let b_deps = &deps_map[b_name];
        match (a_deps.contains(&b_name), b_deps.contains(&a_name)) {
            (true, true) => a_name.cmp(b_name),
            (true, false) => Ordering::Greater,
            (false, true) => Ordering::Less,
            (false, false) => Ordering::Equal
        }
    });

    nodes
}

fn node_depends_on(node: &SerialNode, other_node: &str) -> bool {
    node_dep_nodes(node).any(|dep| dep == other_node)
}

fn node_deps(node: &SerialNode) -> impl Iterator<Item=SerialNodeDep<'_>> {
    node.input_fields.iter()
        .filter_map(|field| extract!(field, SerialFieldElem::Field(field)))
        .flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))
}

fn node_dep_nodes(node: &SerialNode) -> impl Iterator<Item=&str> {
    node_deps(node).map(|dep| dep.node_ident)
}

type ValueDeps<'a> = std::iter::Chain<&'a mut ValueHeadDeps<'a>, ValueChildrenDeps<'a>>;

#[derive(Iterator)]
enum ValueHeadDeps<'a> {
    None(std::iter::Empty<SerialNodeDep<'a>>),
    One(std::iter::Once<SerialNodeDep<'a>>),
    Many(std::iter::FlatMap<std::slice::Iter<'a, SerialValueHead>, ValueHeadDeps<'a>, fn(&SerialValueHead) -> ValueHeadDeps<'_>>)
}

#[derive(Iterator)]
enum ValueChildrenDeps<'a> {
    None(std::iter::Empty<SerialNodeDep<'a>>),
    Many1(std::iter::FlatMap<std::slice::Iter<'a, SerialTupleItem>, ValueDeps<'a>, fn(&SerialTupleItem) -> ValueDeps<'_>>),
    Many2(std::iter::FlatMap<std::slice::Iter<'a, SerialField>, ValueDeps<'a>, fn(&SerialField) -> ValueDeps<'_>>)
}

fn value_deps<'a>(value: Option<&'a SerialValueHead>, value_children: &'a SerialBody) -> ValueDeps<'a> {
    value_head_deps(value).chain(value_children_deps(value_children))
}

fn value_head_deps(value_head: Option<&SerialValueHead>) -> ValueHeadDeps<'_> {
    match value_head {
        None | Some(SerialValueHead::Integer(_)) | Some(SerialValueHead::Float(_)) | Some(SerialValueHead::String(_)) => {
            ValueHeadDeps::None(std::iter::empty())
        },
        Some(SerialValueHead::Ref { node_ident, field_ident }) => {
            ValueHeadDeps::One(std::iter::once(SerialNodeDep { node_ident, field_ident }))
        },
        Some(SerialValueHead::Array(elems)) => {
            ValueHeadDeps::Many(elems.iter().flat_map(|elem| value_head_deps(Some(elem))))
        },
        Some(SerialValueHead::Tuple(elems)) => {
            ValueHeadDeps::Many(elems.iter().flat_map(|elem| value_head_deps(Some(elem))))
        },
    }
}

fn value_children_deps(value_children: &SerialBody) -> ValueChildrenDeps<'_> {
    match value_children {
        SerialBody::None => {
            ValueChildrenDeps::None(std::iter::empty())
        },
        SerialBody::Tuple(elems) => {
            ValueChildrenDeps::Many1(elems.iter().flat_map(|elem| value_deps(elem.value.as_ref(), &elem.value_children)))
        },
        SerialBody::Fields(fields) => {
            ValueChildrenDeps::Many2(fields.iter().flat_map(|field| value_deps(field.value.as_ref(), &field.value_children)))
        }
    }
}