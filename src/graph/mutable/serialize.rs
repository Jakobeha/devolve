use std::any::{Any, TypeId};
use std::collections::HashSet;
use std::iter::zip;
use std::mem::size_of;
use log::error;
use crate::graph::mutable::{FieldHeader, MutableGraph, Node, NodeInput, NodeInputDep, NodeIOType, NodeTypeName};
use crate::graph::parse::types::{SerialBody, SerialField, SerialFieldElem, SerialGraph, SerialNode, SerialRustType, SerialTupleItem, SerialType, SerialValueHead};
use crate::misc::split_balanced::SplitBalanced;
use crate::rust_type::{PrimitiveType, RustType, TypeStructBody, TypeStructField, TypeStructure};

pub struct GraphSerializer {
    result: SerialGraph,
    input_names: Vec<String>,
    node_names_and_output_names: Vec<Option<(String, Vec<String>)>>,
}

impl GraphSerializer {
    pub fn serialize(graph: MutableGraph) -> SerialGraph {
        let mut serializer = GraphSerializer::new();
        serializer._serialize(graph);
        serializer.result
    }

    fn new() -> Self {
        GraphSerializer {
            result: SerialGraph::new(),
            node_names_and_output_names: Vec::new(),
            input_names: Vec::new()
        }
    }

    fn _serialize(&mut self, graph: MutableGraph) {
        // Setup data
        self.input_names.extend(graph.input_types.iter().map(|input_type| input_type.name));

        self.node_names_and_output_names.extend(graph.nodes.iter().map(|(node_id, node)| {
            while self.node_names_and_output_names.len() < node_id {
                self.node_names_and_output_names.push(None);
            }

            let node_name = node.meta.node_name.clone();
            let node_type = &graph.types[&node.type_name];
            let output_names = node_type.outputs.iter().map(|output_type| output_type.name).collect::<Vec<_>>();
            Some((node_name, output_names))
        }));

        // Add nodes
        // Types will also be added as they are inferred by being used in the nodes
        for (_, node) in graph.nodes.into_iter() {
            self.serialize_node(node, &graph);
        }
    }

    fn serialize_node(&mut self, node: Node, graph: &MutableGraph) {
        let serial_node_type = if node.type_name == node.meta.node_name {
            None
        } else {
            Some(node.type_name.to_string())
        };
        let mut serial_node = SerialNode::new(serial_node_type);

        // Add actual fields
        let node_type = &graph.types[&node.type_name];
        debug_assert!(node_type.inputs.len() == node.inputs.len(), "node inputs size mismatch");
        for (input_type, input) in zip(&node_type.inputs, &node.inputs) {
            let serial_field = self.serialize_field(input_type, input);
            serial_node.input_fields.push(SerialFieldElem::Field(serial_field));
        }
        for output_type in &node_type.outputs {
            // There are no output values
            let serial_field = self.serialize_field(output_type, &NodeInput::Hole);
            serial_node.output_fields.push(SerialFieldElem::Field(serial_field));
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
        let (value, value_children) = self.serialize_node_value(field_value, &field_type.rust_type);
        SerialField {
            name: field_type.name.to_string(),
            // TODO: elide type if trivial (e.g. a constant), currently we only elide if we have no idea
            rust_type: self.serialize_rust_type(&field_type.rust_type),
            value,
            value_children
        }
    }

    fn serialize_rust_type(&mut self, rust_type: &RustType) -> Option<SerialRustType> {
        match rust_type {
            RustType::Known(known) => self.serialize_ident(&known.structural_type_name()),
            RustType::Structural(structural) => {
                if structural.starts_with('{') && structural.ends_with('}') {
                    // Anonymous type
                    self.serialize_rust_type_by_structure(&structural.structure)
                } else {
                    // Named type: maybe add the type def but we can refer to it by ident
                    self.add_if_type_def(&structural.type_name, &structural.structure);
                    self.serialize_ident(&structural.type_name)
                }
            }
        }
    }

    fn add_if_type_def(&mut self, type_name: &str, structure: &TypeStructure) {
        if let Some(serial_type_def) = self.serialize_type_def(structure) {
            if self.result.rust_types.contains_key(type_name) {
                // TODO: merge and disambiguate / warn conflicts
            } else {
                self.result.rust_types.insert(type_name.to_string(), serial_type_def);
            }
        }
    }

    fn serialize_rust_type_by_structure(&mut self, structure: &TypeStructure) -> Option<SerialRustType> {
        match structure {
            TypeStructure::Opaque => None,
            // Probably will never be reached
            TypeStructure::Primitive(primitive) => Some(match primitive {
                PrimitiveType::I64 => SerialRustType::Ident { name: "i64".to_string(), generic_args: Vec::new() },
                PrimitiveType::F64 => SerialRustType::Ident { name: "f64".to_string(), generic_args: Vec::new() },
            }),
            TypeStructure::CReprStruct { body } => Some(SerialRustType::Tuple(match body {
                TypeStructBody::None => Vec::new(),
                TypeStructBody::Tuple(elements) => {
                    elements.iter().map(|element| self.serialize_rust_type(element)).collect::<Option<Vec<_>>>()?
                }
                TypeStructBody::Fields(fields) => {
                    fields.iter().map(|field| self.serialize_rust_type(&field.rust_type)).collect::<Option<Vec<_>>>()?
                }
            })),
            // Not expressible (should never be reached anyways)
            TypeStructure::CReprEnum { variants: _ } => None,
            TypeStructure::Tuple { elements } => Some(SerialRustType::Tuple(
                elements.iter().map(|element| self.serialize_rust_type(element)).collect::<Option<Vec<_>>>()?
            )),
            TypeStructure::Pointer { referenced } => Some(SerialRustType::Reference(Box::new(self.serialize_rust_type(referenced)?))),
            TypeStructure::Array { elem, length } => Some(SerialRustType::Array {
                elem: Box::new(self.serialize_rust_type(elem)?),
                length: *length
            }),
            TypeStructure::Slice { elem } => Some(SerialRustType::Slice(
                Box::new(self.serialize_rust_type(elem)?)
            )),
        }
    }

    fn serialize_type_def(&mut self, structure: &TypeStructure) -> Option<SerialType> {

    }

    /// Will only return null iff error:
    ///
    /// - this is an anonymous type (not supposed to be called here)
    /// - the type was unexpectedly-formed
    fn serialize_ident(&mut self, name: &str) -> Option<SerialRustType> {
        if name.starts_with('{') && name.ends_with('}') {
            error!("unexpceted: anonymous type name called in serialize_ident");
            return None;
        }
        if !name.contains('<') && name.ends_with('>') {
            error!("unexpceted: type name ends with '>' but doesn't contain '<'");
            return None;
        }

        let (name, generic_args) = if let Some(name) = name.strip_suffix('>') {
            let (base_name, generic_args_str) = name.split_once('<').unwrap();
            // filter_map will only return null on unexpectedly-formed
            let generic_args = generic_args_str
                .split_balanced(',')
                .filter_map(|arg| self.serialize_ident(arg.trim()))
                .collect::<Vec<_>>();
            (base_name, generic_args)
        } else {
            (name, Vec::new())
        };
        Some(SerialRustType::Ident { name: name.to_string(), generic_args })
    }

    fn serialize_node_value(&mut self, node_value: &NodeInput, rust_type: &RustType) -> (Option<SerialValueHead>, SerialBody) {
        // TODO: Serialize inline tuple / array types if small enough (after calling this we can check by printing)
        match node_value {
            NodeInput::Hole => (None, SerialBody::None),
            NodeInput::Dep(dep) => {
                let (node_name, field_name) = match dep {
                    NodeInputDep::GraphInput { idx } => {
                        (String::from(NodeTypeName::INPUT), self.input_names[idx].clone())
                    }
                    NodeInputDep::OtherNodeOutput { id, idx } => {
                        let (node_name, output_names) = self.node_names_and_output_names[id].unwrap();
                        (node_name.clone(), output_names[idx].clone())
                    }
                };
                (Some(SerialValueHead::Ref { node_name, field_name }), SerialBody::None)
            },
            NodeInput::Const(constant_data) => {
                self.serialize_constant(constant_data, rust_type)
            }
            NodeInput::Array(elems) => {
                (None, SerialBody::Tuple(elems.iter().map(|e| self.serialize_node_value(e, rust_type.array_elem_type)).collect()))
            }
            NodeInput::Tuple(tuple_items) => {
                (None, SerialBody::Tuple(tuple_items.iter().map(|e| self.serialize_node_value(e, rust_type.tuple_elem_type)).collect()))
            }
        }
    }

    fn serialize_constant(&mut self, constant_data: &[u8], rust_type: &RustType) -> (Option<SerialValueHead>, SerialBody) {
        let size = rust_type.infer_size();
        if size.is_none() {
            error!("deserialized constant data is of type with unknown size: actual size of data = {}, type = {}", constant_data.len(), rust_type);
            return (None, SerialBody::None)
        } else if size.unwrap() != constant_data.len() {
            error!("deserialized constant data doesn't match type size: size = {}, type = {} (size {})", constant_data.len(), rust_type, size.unwrap());
            return (None, SerialBody::None)
        }

        match rust_type {
            RustType::Known(known) => match known.intrinsic.type_id() {
                TypeId::of::<i64>() => {
                    let mut constant_data_bytes = [0; size_of::<i64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = i64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Integer(value)), SerialBody::None)
                }
                TypeId::of::<f64>() => {
                    let mut constant_data_bytes = [0; size_of::<f64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = f64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Float(value)), SerialBody::None)
                }
                TypeId::of::<String>() => {
                    // Should be guaranteed utf-8 but just for safety
                    let value = String::from_utf8_lossy(constant_data).into_owned();
                    (Some(SerialValueHead::String(value)), SerialBody::None)
                }
                _ => {
                    error!("deserialized constant data is of unknown intrinsic type: {}", rust_type);
                    (None, SerialBody::None)
                }
            }
            RustType::Structural(structural) => (None, match &structural.structure {
                TypeStructure::CReprStruct { body } => match body {
                    TypeStructBody::None => SerialBody::None,
                    TypeStructBody::Tuple(tuple_item_types) => {
                        self.serialize_constant_tuple(constant_data, tuple_item_types)
                    }
                    TypeStructBody::Fields(field_types) => {
                        self.serialize_constant_fields(constant_data, field_types)
                    }
                },
                TypeStructure::Tuple { elements} => {
                    self.serialize_constant_tuple(constant_data, elements)
                }
                TypeStructure::Array { elem, length } => {
                    self.serialize_constant_array(constant_data, &elem, *length)
                }
            })
        }
    }

    fn serialize_constant_tuple(&mut self, constant_data: &[u8], elem_types: &[RustType]) -> SerialBody {
        let tuple_items = match self.serialize_constant_compound(
            constant_data,
            tuple_item_types.iter().map(|tuple_item_type| (tuple_item_type, ())),
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

    fn serialize_constant_fields(&mut self, constant_data: &[u8], field_types: &[TypeStructField]) -> SerialBody {
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

    fn serialize_constant_compound<U, V, It: Iterator<Item=(&RustType, U)>>(
        &mut self,
        constant_data: &[u8],
        elem_types: It,
        mk_serial_elem: fn(Option<SerialRustType>, Option<SerialValueHead>, SerialBody, U) -> V
    ) -> Option<Vec<V>> {
        let mut current_size = 0;
        let mut elems = Vec::new();
        for (elem_type, elem_assoc) in elem_types {
            let elem_size = elem_type.infer_size();
            let elem_align = elem_type.infer_align();
            if elem_size.is_none() || elem_align.is_none() {
                error!("deserialized constant data contains type with unknown size: unsized type = {}, outer type = {}", elem_type, rust_type);
                return None
            }
            let elem_size = elem_size.unwrap();
            let elem_align = elem_align.unwrap();

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
}