use std::iter::{repeat_with, zip};
use std::mem::size_of;

use join_lazy_fmt::Join;
use log::{error, warn};

use crate::graph::mutable::{FieldHeader, MutableGraph, Node, NodeInput, NodeInputDep, NodeInputWithLayout, NodeIOType, NodeTypeData};
use crate::graph::parse::types::{SerialBody, SerialEnumTypeDef, SerialEnumVariantTypeDef, SerialField, SerialFieldElem, SerialFieldTypeDef, SerialGraph, SerialNode, SerialRustType, SerialStructTypeDef, SerialTupleItem, SerialTypeDef, SerialTypeDefBody, SerialValueHead};
use crate::graph::StaticStrs;
use crate::mutable::ComptimeCtx;
use structural_reflection::{DuplicateNamesInScope, PrimitiveType, RustType, RustTypeName, TypeEnumVariant, TypeStructureBody, TypeStructureBodyField, TypeStructure};

pub struct GraphSerializer<'a> {
    /// Only actually needs some of the ctx, GraphBuilder needs more, but we use the same struct
    /// because they are close enough and we'll have the data
    ctx: &'a ComptimeCtx,
    result: SerialGraph,
    rust_type_names: DuplicateNamesInScope,
    input_names: Vec<String>,
    node_names_and_output_names: Vec<Option<(String, Vec<String>)>>,
}

impl<'a> GraphSerializer<'a> {
    pub fn serialize(graph: MutableGraph, ctx: &'a ComptimeCtx, additional_type_defs: impl Iterator<Item=&'a (String, TypeStructure)>) -> SerialGraph {
        let mut serializer = GraphSerializer::new(ctx);
        serializer.add_type_defs(additional_type_defs);
        serializer._serialize(graph);
        serializer.result
    }

    fn new(ctx: &'a ComptimeCtx) -> Self {
        GraphSerializer {
            ctx,
            result: SerialGraph::new(),
            rust_type_names: DuplicateNamesInScope::new(),
            node_names_and_output_names: Vec::new(),
            input_names: Vec::new()
        }
    }

    fn add_type_defs(&mut self, additional_type_defs: impl Iterator<Item=&'a (String, TypeStructure)>) {
        for (type_name, type_def) in additional_type_defs {
            let added = self.add_if_type_def2(type_name, type_def);
            if !added {
                error!("didn't add additional type def because it's not a struct or enum: {}", type_name);
            }
        }
    }

    fn _serialize(&mut self, graph: MutableGraph) {
        // Setup data
        // Don't need to iterate nested rust types or structures, because any type names are only in the surface types
        self.rust_type_names.extend(graph.iter_rust_types().flat_map(|rust_type| rust_type.type_name.iter_simple_names()));
        self.input_names.extend(graph.input_types.iter().map(|input_type| input_type.name.clone()));

        self.node_names_and_output_names.reserve(graph.nodes.capacity());
        for (node_id, node) in &graph.nodes {
            while self.node_names_and_output_names.len() < node_id {
                self.node_names_and_output_names.push(None);
            }

            let node_name = node.meta.node_name.clone();
            let node_type = &graph.types[&node.type_name];
            let output_names = node_type.outputs.iter().map(|output_type| output_type.name.clone()).collect::<Vec<_>>();
            self.node_names_and_output_names.push(Some((node_name, output_names)))
        }

        // Add nodes
        // Types will be inferred from the nodes
        for (_, node) in graph.nodes.into_iter() {
            let node_type = &graph.types[&node.type_name];
            self.serialize_node(node, &node_type);
        }
    }

    fn serialize_node(&mut self, node: Node, node_type: &NodeTypeData) {
        let serial_node_type = if node.type_name.as_ref() == node.meta.node_name.as_str() {
            None
        } else {
            Some(node.type_name.to_string())
        };
        let mut serial_node = SerialNode::new(serial_node_type);

        // Add actual fields
        debug_assert!(node_type.inputs.len() == node.inputs.len(), "node inputs size mismatch");
        for (input_type, input) in zip(&node_type.inputs, &node.inputs) {
            let serial_field = self.serialize_field(input_type, input);
            serial_node.input_fields.push(SerialFieldElem::Field { field: serial_field });
        }
        for output_type in &node_type.outputs {
            // There are no output values
            let serial_field = self.serialize_field(output_type, &NodeInput::Hole);
            serial_node.output_fields.push(SerialFieldElem::Field { field: serial_field });
        }

        // Add headers
        // Note: We want to do this in order even though later indices are affected by earlier inserts,
        // that is how it is created
        for FieldHeader { header, index } in node.meta.input_headers {
            serial_node.input_fields.insert(index, SerialFieldElem::Header { header });
        }
        for FieldHeader { header, index } in node.meta.output_headers {
            serial_node.output_fields.insert(index, SerialFieldElem::Header { header });
        }

        // Add node to result
        self.result.nodes.insert(node.meta.node_name, serial_node);
    }

    fn serialize_field(&mut self, field_type: &NodeIOType, field_value: &NodeInput) -> SerialField {
        let (mut value, mut value_children) = self.serialize_node_value(field_value, &field_type.rust_type);
        self.inline_value_if_small_enough((&mut value, &mut value_children));
        let rust_type = self.elide_type_if_trivial((value.as_ref(), &value_children), |this| {
            this.serialize_rust_type(&field_type.rust_type)
        });
        SerialField {
            name: field_type.name.to_string(),
            rust_type,
            value,
            value_children
        }
    }

    fn serialize_rust_type(&mut self, rust_type: &RustType) -> Option<SerialRustType> {
        if rust_type.type_id.is_some() {
            // Known
            // Still need to remove qualifiers on generic args
            Some(self.serialize_rust_type_name(rust_type.type_name.clone()))
        } else if rust_type.type_name.is_anonymous() {
            // Anonymous
            self.serialize_rust_type_by_structure(&rust_type.structure)
        } else {
            // Named, may be user defined
            self.add_if_type_def(rust_type);
            Some(self.serialize_rust_type_name(rust_type.type_name.clone()))
        }
    }

    fn add_if_type_def(&mut self, rust_type: &RustType) -> bool {
        match &rust_type.type_name {
            RustTypeName::Ident { qualifiers, simple_name, generic_args }
            // Generic args are only supported for builtins.
            if qualifiers == &self.ctx.qualifiers && generic_args.is_empty() => {
                self.add_if_type_def2(simple_name, &rust_type.structure)
            }
            _ => false
        }
    }

    fn add_if_type_def2(&mut self, type_name: &str, structure: &TypeStructure) -> bool {
        match self.serialize_type_def(structure) {
            None => false,
            Some(serial_type_def) => {
                if let Some((type_name, mut old_serial_type_def)) = self.result.rust_types.remove_entry(type_name) {
                    let mut errors = Vec::new();
                    self.merge_type_defs(&mut old_serial_type_def, serial_type_def, &mut errors);
                    if !errors.is_empty() {
                        error!("conflicting inferred type def. Errors:\n\t{}", "\n\t".join(errors));
                    }
                    self.result.rust_types.insert(type_name, old_serial_type_def);
                } else {
                    self.result.rust_types.insert(type_name.to_string(), serial_type_def);
                }
                true
            }
        }
    }

    // Don't use &mut even though we can to simulate converting types
    fn serialize_rust_type_name(&mut self, mut rust_type_name: RustTypeName) -> SerialRustType {
        rust_type_name.remove_qualifier(&self.ctx.qualifiers);
        rust_type_name
    }

    fn serialize_rust_type_by_structure(&mut self, structure: &TypeStructure) -> Option<SerialRustType> {
        match structure {
            TypeStructure::Opaque => None,
            // Probably will never be reached
            TypeStructure::Primitive(primitive) => Some(primitive.rust_type_name()),
            TypeStructure::CReprStruct { .. } |
            TypeStructure::CReprEnum { .. } |
            TypeStructure::Pointer { .. } => unreachable!("should not have anonymous structs, enums, or pointers"),
            TypeStructure::CTuple { elements } => Some(SerialRustType::Tuple {
                elems: elements.iter().map(|element| self.serialize_rust_type(element)).collect::<Option<Vec<_>>>()?
            }),
            TypeStructure::Array { elem, length } => Some(SerialRustType::Array {
                elem: Box::new(self.serialize_rust_type(elem)?),
                length: *length
            }),
            TypeStructure::Slice { elem } => Some(SerialRustType::Slice {
                elem: Box::new(self.serialize_rust_type(elem)?)
            }),
        }
    }

    fn serialize_type_def(&mut self, structure: &TypeStructure) -> Option<SerialTypeDef> {
        match structure {
            TypeStructure::CReprStruct { body } => {
                Some(SerialTypeDef::Struct(SerialStructTypeDef { body: self.serialize_type_def_body(body) }))
            },
            TypeStructure::CReprEnum { variants } => {
                Some(SerialTypeDef::Enum(SerialEnumTypeDef { variants: self.serialize_type_def_variants(variants) }))
            },
            _ => None
        }
    }

    fn serialize_type_def_body(&mut self, body: &TypeStructureBody) -> SerialTypeDefBody {
        match body {
            TypeStructureBody::None => SerialTypeDefBody::None,
            TypeStructureBody::Tuple(elements) => {
                SerialTypeDefBody::Tuple(elements.iter().map(|element| {
                    self.serialize_rust_type(element).unwrap_or(SerialRustType::unknown())
                }).collect::<Vec<_>>())
            },
            TypeStructureBody::Fields(fields) => {
                SerialTypeDefBody::Fields(fields.iter().map(|TypeStructureBodyField { name, rust_type }| {
                    SerialFieldTypeDef {
                        name: name.to_string(),
                        rust_type: self.serialize_rust_type(&rust_type),
                        // Default values are currently unsupported and only a feature of parsing
                        default_value: None,
                        default_value_children: SerialBody::None
                    }
                }).collect::<Vec<_>>())
            }
        }
    }

    fn serialize_type_def_variants(&mut self, variants: &[TypeEnumVariant]) -> Vec<SerialEnumVariantTypeDef> {
        variants.iter().map(|TypeEnumVariant { variant_name: name, body }| {
            SerialEnumVariantTypeDef {
                name: name.clone(),
                body: self.serialize_type_def_body(body)
            }
        }).collect::<Vec<_>>()
    }

    fn merge_type_defs(&mut self, old_serial_type_def: &mut SerialTypeDef, new_serial_type_def: SerialTypeDef, errors: &mut Vec<String>) {
        match (old_serial_type_def, new_serial_type_def) {
            (SerialTypeDef::Struct(old_struct), SerialTypeDef::Struct(new_struct)) => {
                self.merge_type_def_bodies(&mut old_struct.body, new_struct.body, errors)
            }
            (SerialTypeDef::Enum(old_enum), SerialTypeDef::Enum(new_enum)) => {
                self.merge_enum_type_defs(old_enum, new_enum, errors)
            }
            _ => {
                errors.push(String::from("one is a struct and the other is an enum"));
            }
        }
    }

    fn merge_type_def_bodies(&mut self, old_body: &mut SerialTypeDefBody, new_body: SerialTypeDefBody, errors: &mut Vec<String>) {
        match (old_body, new_body) {
            (SerialTypeDefBody::None, SerialTypeDefBody::None) => {
                // Nothing to do
            }
            (SerialTypeDefBody::Tuple(old_tuple), SerialTypeDefBody::Tuple(new_tuple)) => {
                self.merge_rust_type_slices(old_tuple, new_tuple, errors)
            }
            (SerialTypeDefBody::Fields(old_fields), SerialTypeDefBody::Fields(new_fields)) => {
                self.merge_fields(old_fields, new_fields, errors)
            }
            _ => {
                errors.push(String::from("different nested struct body types"));
            }
        }
    }

    fn merge_enum_type_defs(&mut self, old_enum: &mut SerialEnumTypeDef, new_enum: SerialEnumTypeDef, errors: &mut Vec<String>) {
        if old_enum.variants.len() != new_enum.variants.len() {
            errors.push(String::from("different enum variants"));
        } else {
            for (old_variant, new_variant) in old_enum.variants.iter_mut().zip(new_enum.variants.into_iter()) {
                if old_variant.name != new_variant.name {
                    errors.push(String::from("different enum variant names"));
                } else {
                    self.merge_type_def_bodies(&mut old_variant.body, new_variant.body, errors)
                }
            }
        }
    }

    fn merge_rust_type_slices(&mut self, old_tuple_items: &mut [SerialRustType], new_tuple_items: Vec<SerialRustType>, errors: &mut Vec<String>) {
        if old_tuple_items.len() != new_tuple_items.len() {
            errors.push(String::from("tuple lengths do not match"));
        } else {
            for (old_tuple_item, new_tuple_item) in old_tuple_items.iter_mut().zip(new_tuple_items.into_iter()) {
                self.merge_rust_types(old_tuple_item, new_tuple_item, errors);
            }
        }
    }

    fn merge_fields(&mut self, old_fields: &mut [SerialFieldTypeDef], new_fields: Vec<SerialFieldTypeDef>, errors: &mut Vec<String>) {
        if old_fields.len() != new_fields.len() {
            errors.push(String::from("field lengths do not match"));
        } else {
            for (old_field, new_field) in old_fields.iter_mut().zip(new_fields.into_iter()) {
                self.merge_field_types(old_field, new_field, errors);
            }
        }
    }

    fn merge_field_types(&mut self, old_field: &mut SerialFieldTypeDef, new_field: SerialFieldTypeDef, errors: &mut Vec<String>) {
        if old_field.name != new_field.name {
            errors.push(String::from("field names do not match"));
        } else {
            match (&mut old_field.rust_type, new_field.rust_type) {
                (None, new_field_type) => old_field.rust_type = new_field_type,
                (Some(_), None) => {},
                (Some(old_field_type), Some(new_field_type)) => {
                    self.merge_rust_types(old_field_type, new_field_type, errors)
                }
            }
        }
    }

    fn merge_rust_types(&mut self, old_type: &mut SerialRustType, new_type: SerialRustType, errors: &mut Vec<String>) {
        if old_type.is_unknown() {
            *old_type = new_type;
        } else if new_type.is_unknown() {
            // ignore
        } else {
            match (old_type, new_type) {
                (SerialRustType::Pointer { ptr_kind: old_ptr_kind, refd: old_ref }, SerialRustType::Pointer { ptr_kind: new_ptr_kind, refd: new_ref }) => {
                    self.merge_rust_types(old_ref, *new_ref, errors);
                    if *old_ptr_kind != new_ptr_kind {
                        errors.push(String::from("different pointer kinds"));
                    }
                }
                (SerialRustType::Array { elem, length }, SerialRustType::Array { elem: new_elem, length: new_length }) => {
                    self.merge_rust_types(elem, *new_elem, errors);
                    if *length != new_length {
                        errors.push(String::from("array lengths do not match"));
                    }
                }
                (SerialRustType::Slice { elem: old_elem }, SerialRustType::Slice { elem: new_elem }) => {
                    self.merge_rust_types(old_elem, *new_elem, errors);
                }
                (SerialRustType::Tuple { elems: old_elems }, SerialRustType::Tuple { elems: new_elems }) => {
                    self.merge_rust_type_slices(old_elems, new_elems, errors);
                }
                (SerialRustType::Ident { qualifiers, simple_name, generic_args }, SerialRustType::Ident { qualifiers: new_qualifiers, simple_name: new_simple_name, generic_args: new_generic_args }) => {
                    if qualifiers != &new_qualifiers {
                        errors.push(String::from("ident qualifiers do not match"));
                    }
                    if simple_name != &new_simple_name {
                        errors.push(String::from("ident names do not match"));
                    }
                    self.merge_rust_type_slices(generic_args, new_generic_args, errors);
                }
                _ => {
                    errors.push(String::from("different rust types"));
                }
            }
        }
    }

    fn serialize_node_value(&mut self, node_value: &NodeInput, rust_type: &RustType) -> (Option<SerialValueHead>, SerialBody) {
        match node_value {
            NodeInput::Hole => (None, SerialBody::None),
            NodeInput::Dep(dep) => {
                let (node_name, field_name) = match dep {
                    NodeInputDep::GraphInput { idx } => {
                        (String::from(StaticStrs::INPUT_NODE), self.input_names[*idx].clone())
                    }
                    NodeInputDep::OtherNodeOutput { id, idx } => {
                        let (node_name, output_names) = self.node_names_and_output_names[id.0].as_ref().unwrap();
                        (node_name.clone(), output_names[*idx].clone())
                    }
                };
                (Some(SerialValueHead::Ref { node_name, field_name }), SerialBody::None)
            },
            NodeInput::Const(constant_data) => {
                self.serialize_constant(constant_data, rust_type)
            }
            NodeInput::Array(elems) => {
                let array_elem_type = rust_type.structure.array_elem_type_and_length().map(|(array_elem_type, length)| {
                    if length != elems.len() {
                        error!("array length mismatch: {} != {}", length, elems.len());
                    }
                    array_elem_type
                });
                (None, SerialBody::Tuple(elems.iter().map(|elem| {
                    let rust_type = array_elem_type.and_then(|array_elem_type| self.serialize_rust_type(array_elem_type));
                    let (value, value_children) = self.serialize_node_value(elem, array_elem_type.unwrap_or(&RustType::unknown()));
                    SerialTupleItem {
                        rust_type,
                        value,
                        value_children
                    }
                }).collect()))
            }
            NodeInput::Tuple(tuple_items) => match &rust_type.structure {
                TypeStructure::CReprStruct { body: body_type } => {
                    let head = SerialValueHead::Struct {
                        type_name: rust_type.type_name.clone(),
                        inline_params: None
                    };
                    let body = self.serialize_body(body_type, tuple_items);
                    (Some(head), body)
                }
                TypeStructure::CReprEnum { variants } => {
                    if variants.len() == 0 {
                        error!("deserialized 'bottom' value, something is clearly wrong");
                        return (None, SerialBody::None);
                    } else if variants.len() != 1 {
                        warn!("deserializing tuple with multiple variants, don't know which one to pick");
                    }
                    let variant_type = &variants[0];

                    let head = SerialValueHead::Enum {
                        type_name: rust_type.type_name.clone(),
                        variant_name: variant_type.variant_name.clone(),
                        inline_params: None
                    };
                    let body = self.serialize_body(&variant_type.body, tuple_items);
                    (Some(head), body)
                }
                TypeStructure::CTuple { elements: tuple_item_types } => {
                    let body = self.serialize_tuple_body(tuple_item_types, tuple_items);
                    (None, body)
                }
                _ => {
                    warn!("deserializing tuple with unknown item types");
                    let mut tuple_item_types = Vec::with_capacity(tuple_items.len());
                    tuple_item_types.extend(repeat_with(RustType::unknown).take(tuple_items.len()));
                    let body = self.serialize_tuple_body(&tuple_item_types, tuple_items);
                    (None, body)
                }
            }
        }
    }

    fn serialize_constant(&mut self, constant_data: &[u8], rust_type: &RustType) -> (Option<SerialValueHead>, SerialBody) {
        let size = rust_type.size;
        if size != constant_data.len() {
            error!("deserialized constant data doesn't match type size: size = {}, type = {} (size {})", constant_data.len(), rust_type.type_name.unqualified(), size);
            return (None, SerialBody::None)
        }

        match &rust_type.structure {
            TypeStructure::Primitive(primitive) => match primitive {
                PrimitiveType::I64 => {
                    let mut constant_data_bytes = [0; size_of::<i64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = i64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Integer(value)), SerialBody::None)
                }
                PrimitiveType::F64 => {
                    let mut constant_data_bytes = [0; size_of::<f64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = f64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Float(value)), SerialBody::None)
                }
                _ => todo!("primitives which aren't i64 and f64 not yet implemented")
            }
            TypeStructure::CReprStruct { body } => (None, match body {
                TypeStructureBody::None => SerialBody::None,
                TypeStructureBody::Tuple(tuple_item_types) => {
                    self.serialize_constant_tuple(constant_data, tuple_item_types)
                }
                TypeStructureBody::Fields(field_types) => {
                    self.serialize_constant_fields(constant_data, field_types)
                }
            }),
            TypeStructure::Opaque => {
                error!("deserialized constant data is of opaque type, don't know how to interpret it");
                (None, SerialBody::None)
            }
            TypeStructure::CReprEnum { .. } => {
                error!("deserialized constant data is of enum type, which can't be represented in serial data");
                (None, SerialBody::None)
            }
            TypeStructure::Pointer { .. } => {
                error!("deserialized constant data is of pointer type, which can't be represented in serial data");
                (None, SerialBody::None)
            }
            TypeStructure::CTuple { elements} => {
                (None, self.serialize_constant_tuple(constant_data, elements))
            }
            TypeStructure::Array { elem, length } => {
                (None, self.serialize_constant_array(constant_data, &elem, *length))
            }
            TypeStructure::Slice { elem } => {
                (None, self.serialize_constant_slice(constant_data, &elem))
            }
        }
    }

    fn serialize_constant_tuple(&mut self, constant_data: &[u8], item_types: &[RustType]) -> SerialBody {
        let tuple_items = match self.serialize_constant_compound(
            constant_data,
            item_types.iter().map(|item_type| (item_type, ())),
            |serial_rust_type, serial_value, serial_value_children, ()| {
                SerialTupleItem {
                    rust_type: serial_rust_type,
                    value: serial_value,
                    value_children: serial_value_children
                }
            }
        ) {
            None => return SerialBody::None,
            Some(tuple_items) => tuple_items
        };
        SerialBody::Tuple(tuple_items)
    }

    fn serialize_constant_fields(&mut self, constant_data: &[u8], field_types: &[TypeStructureBodyField]) -> SerialBody {
        let fields = match self.serialize_constant_compound(
            constant_data,
            field_types.iter().map(|field_type| (&field_type.rust_type, field_type.name.clone())),
            |serial_rust_type, serial_value, serial_value_children, name| {
                SerialField {
                    name,
                    rust_type: serial_rust_type,
                    value: serial_value,
                    value_children: serial_value_children
                }
            }
        ) {
            None => return SerialBody::None,
            Some(fields) => fields
        };
        SerialBody::Fields(fields)
    }

    fn serialize_constant_array(&mut self, constant_data: &[u8], element_type: &RustType, len: usize) -> SerialBody {
        let elements = match self.serialize_constant_compound(
            constant_data,
            std::iter::repeat(element_type).map(|element_type| (element_type, ())).take(len),
            |serial_rust_type, serial_value, serial_value_children, ()| {
                SerialTupleItem {
                    rust_type: serial_rust_type,
                    value: serial_value,
                    value_children: serial_value_children
                }
            }
        ) {
            None => return SerialBody::None,
            Some(elements) => elements
        };
        SerialBody::Tuple(elements)
    }

    fn serialize_constant_slice(&mut self, constant_data: &[u8], element_type: &RustType) -> SerialBody {
        let length = constant_data.len() / element_type.size;
        self.serialize_constant_array(constant_data, element_type, length)
    }

    fn serialize_constant_compound<'b, U, V, It: Iterator<Item=(&'b RustType, U)>>(
        &mut self,
        constant_data: &[u8],
        elem_types: It,
        mk_serial_elem: fn(Option<SerialRustType>, Option<SerialValueHead>, SerialBody, U) -> V
    ) -> Option<Vec<V>> {
        let mut current_size = 0;
        let mut elems = Vec::new();
        for (elem_type, elem_assoc) in elem_types {
            let elem_size = elem_type.size;
            let elem_align = elem_type.align;

            if current_size % elem_align != 0 {
                current_size += elem_align - current_size % elem_align;
            }
            let constant_slice = &constant_data[current_size..current_size + elem_size];
            let (elem_value, elem_value_children) = self.serialize_constant(constant_slice, elem_type);
            current_size += elem_size;

            let elem_rust_type = self.serialize_rust_type(elem_type);

            elems.push(mk_serial_elem(elem_rust_type, elem_value, elem_value_children, elem_assoc));
        }
        Some(elems)
    }

    fn serialize_body(&mut self, body_type: &TypeStructureBody, tuple_items: &[NodeInputWithLayout]) -> SerialBody {
        match body_type {
            TypeStructureBody::None => SerialBody::None,
            TypeStructureBody::Tuple(tuple_item_types) => {
                self.serialize_tuple_body(tuple_item_types, tuple_items)
            }
            TypeStructureBody::Fields(field_types) => {
                self.serialize_field_body(field_types, tuple_items)
            }
        }
    }

    fn serialize_tuple_body(&mut self, tuple_item_types: &[RustType], tuple_items: &[NodeInputWithLayout]) -> SerialBody {
        SerialBody::Tuple(zip(tuple_item_types.iter(), tuple_items.iter()).map(|(tuple_item_type, NodeInputWithLayout { input: tuple_item, .. })| {
            let rust_type = self.serialize_rust_type(tuple_item_type);
            let (value, value_children) = self.serialize_node_value(tuple_item, tuple_item_type);
            SerialTupleItem {
                rust_type,
                value,
                value_children
            }
        }).collect())
    }

    fn serialize_field_body(&mut self, field_types: &[TypeStructureBodyField], fields: &[NodeInputWithLayout]) -> SerialBody {
        SerialBody::Fields(zip(field_types.iter(), fields.iter()).map(|(field_type, NodeInputWithLayout { input: field, .. })| {
            let rust_type = self.serialize_rust_type(&field_type.rust_type);
            let (value, value_children) = self.serialize_node_value(field, &field_type.rust_type);
            SerialField {
                name: field_type.name.clone(),
                rust_type,
                value,
                value_children
            }
        }).collect())
    }

    fn inline_value_if_small_enough(
        &self,
        (_value, _value_children): (&mut Option<SerialValueHead>, &mut SerialBody)
    ) {
        // TODO
    }

    fn elide_type_if_trivial(
        &mut self,
        (value, value_children): (Option<&SerialValueHead>, &SerialBody),
        get_type: impl FnOnce(&mut Self) -> Option<SerialRustType>
    ) -> Option<SerialRustType> {
        if self.is_type_trivial((value, value_children)) {
            None
        } else {
            get_type(self)
        }
    }

    fn is_type_trivial(&self, (value, value_children): (Option<&SerialValueHead>, &SerialBody)) -> bool {
        match value {
            Some(SerialValueHead::Integer(_)) |
            Some(SerialValueHead::Float(_)) |
            Some(SerialValueHead::String(_)) |
            Some(SerialValueHead::Struct { .. }) |
            Some(SerialValueHead::Enum { .. }) => true,
            Some(SerialValueHead::Ref { .. }) => false,
            Some(SerialValueHead::Tuple(tuple_items)) => {
                tuple_items.iter().all(|tuple_item| {
                    self.is_type_trivial((Some(tuple_item), &SerialBody::None))
                })
            },
            Some(SerialValueHead::Array(array_items)) => {
                array_items.iter().any(|array_item| {
                    self.is_type_trivial((Some(array_item), &SerialBody::None))
                })
            },
            None => match value_children {
                SerialBody::None => false,
                SerialBody::Tuple(tuple_items) => {
                    tuple_items.iter().all(|tuple_item| {
                        tuple_item.rust_type.is_some() ||
                            self.is_type_trivial((tuple_item.value.as_ref(), &tuple_item.value_children))
                    })
                },
                SerialBody::Fields(fields) => {
                    fields.iter().all(|field| {
                        field.rust_type.is_some() ||
                            self.is_type_trivial((field.value.as_ref(), &field.value_children))
                    })
                }
            }
        }
    }
}