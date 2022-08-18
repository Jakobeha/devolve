use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

//noinspection RsUnusedImport (intelliJ fails to detect SerialFieldElem use)
use crate::graph::parse::types::{SerialBody, SerialFieldElem, SerialNode, SerialValueHead};
use crate::misc::extract::extract;

struct SerialNodeDep<'a> {
    pub node_ident: &'a str,
    pub field_ident: &'a str
}

pub fn sort_nodes_by_deps(nodes: HashMap<String, SerialNode>) -> Vec<(String, SerialNode)> {
    let deps_map = nodes.iter()
        .map(|(node_name, node)| (node_name.to_string(), node_dep_nodes(node).map(String::from).collect::<HashSet<_>>()))
        .collect::<HashMap<_, _>>();
    let mut nodes = nodes.into_iter().collect::<Vec<_>>();

    nodes.sort_by(|(a_name, _), (b_name, _)| {
        let a_deps = &deps_map[a_name];
        let b_deps = &deps_map[b_name];
        match (a_deps.contains(b_name.as_str()), b_deps.contains(a_name.as_str())) {
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

fn value_deps<'a>(value: Option<&'a SerialValueHead>, value_children: &'a SerialBody) -> impl Iterator<Item=SerialNodeDep<'a>> {
    value_head_deps(value).chain(value_children_deps(value_children))
}

fn value_head_deps(value_head: Option<&SerialValueHead>) -> impl Iterator<Item=SerialNodeDep> {
    match value_head {
        None | Some(SerialValueHead::Integer(_)) | Some(SerialValueHead::Float(_)) | Some(SerialValueHead::String(_)) => {
            Box::new(std::iter::empty()) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Ref { node_ident, field_ident }) => {
            Box::new(std::iter::once(SerialNodeDep { node_ident, field_ident })) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Array(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        Some(SerialValueHead::Tuple(elems)) => {
            Box::new(elems.iter().flat_map(|elem| value_head_deps(Some(elem)))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
    }
}

fn value_children_deps(value_children: &SerialBody) -> impl Iterator<Item=SerialNodeDep> {
    match value_children {
        SerialBody::None => {
            Box::new(std::iter::empty()) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        SerialBody::Tuple(elems) => {
            Box::new(elems.iter().flat_map(|elem| value_deps(elem.value.as_ref(), &elem.value_children))) as Box<dyn Iterator<Item=SerialNodeDep>>
        },
        SerialBody::Fields(fields) => {
            Box::new(fields.iter().flat_map(|field| value_deps(field.value.as_ref(), &field.value_children))) as Box<dyn Iterator<Item=SerialNodeDep>>
        }
    }
}