use std::any::{Any, TypeId};
use std::iter::zip;
use std::mem::size_of;
use log::error;
use join_lazy_fmt::Join;
use crate::graph::mutable::{FieldHeader, MutableGraph, Node, NodeInput, NodeInputDep, NodeInputWithLayout, NodeIOType, NodeTypeData, NodeTypeName};
use crate::graph::parse::types::{SerialBody, SerialEnumType, SerialEnumVariantType, SerialField, SerialFieldElem, SerialFieldType, SerialGraph, SerialNode, SerialRustType, SerialStructType, SerialTupleItem, SerialType, SerialTypeBody, SerialValueHead};
use crate::misc::split_balanced::{ParenType, SplitBalanced};
use crate::rust_type::{PrimitiveType, RustType, StructuralRustType, TypeEnumVariant, TypeStructBody, TypeStructField, TypeStructure};

pub struct GraphSerializer {
    result: SerialGraph,
    input_names: Vec<String>,
    node_names_and_output_names: Vec<Option<(String, Vec<String>)>>,
}

impl GraphSerializer {
    pub fn serialize<'a>(graph: MutableGraph, additional_type_defs: impl Iterator<Item=&'a StructuralRustType>) -> SerialGraph {
        let mut serializer = GraphSerializer::new();
        serializer.add_type_defs(additional_type_defs);
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

    fn add_type_defs<'a>(&mut self, additional_type_defs: impl Iterator<Item=&'a StructuralRustType>) {
        for additional_type_def in additional_type_defs {
            self.add_if_type_def(&additional_type_def.type_name, &additional_type_def.structure);
        }
    }

    fn _serialize(&mut self, graph: MutableGraph) {
        // Setup data
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
            RustType::Known(known) => Some(self.serialize_ident(&known.structural_type_name())),
            RustType::Structural(structural) => {
                if structural.type_name.starts_with('{') && structural.type_name.ends_with('}') {
                    // Anonymous type
                    self.serialize_rust_type_by_structure(&structural.structure)
                } else {
                    // Named type: maybe add the type def but we can refer to it by ident
                    self.add_if_type_def(&structural.type_name, &structural.structure);
                    Some(self.serialize_ident(&structural.type_name))
                }
            }
        }
    }

    fn add_if_type_def(&mut self, type_name: &str, structure: &TypeStructure) {
        if let Some(serial_type_def) = self.serialize_type_def(structure) {
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
        match structure {
            TypeStructure::CReprStruct { body } => {
                Some(SerialType::Struct(SerialStructType { body: self.serialize_type_def_body(body) }))
            },
            TypeStructure::CReprEnum { variants } => {
                Some(SerialType::Enum(SerialEnumType { variants: self.serialize_type_def_variants(variants) }))
            },
            _ => None
        }
    }

    fn serialize_type_def_body(&mut self, body: &TypeStructBody) -> SerialTypeBody {
        match body {
            TypeStructBody::None => SerialTypeBody::None,
            TypeStructBody::Tuple(elements) => {
                SerialTypeBody::Tuple(elements.iter().map(|element| {
                    self.serialize_rust_type(element).unwrap_or(SerialRustType::unknown())
                }).collect::<Vec<_>>())
            },
            TypeStructBody::Fields(fields) => {
                SerialTypeBody::Fields(fields.iter().map(|TypeStructField { name, rust_type }| {
                    SerialFieldType {
                        name: name.to_string(),
                        rust_type: self.serialize_rust_type(&rust_type),
                        default_value: None,
                        default_value_children: Default::default()
                    }
                }).collect::<Vec<_>>())
            }
        }
    }

    fn serialize_type_def_variants(&mut self, variants: &[TypeEnumVariant]) -> Vec<SerialEnumVariantType> {
        variants.iter().map(|TypeEnumVariant { name, body }| {
            SerialEnumVariantType {
                name: name.clone(),
                body: self.serialize_type_def_body(body)
            }
        }).collect::<Vec<_>>()
    }

    fn merge_type_defs(&mut self, old_serial_type_def: &mut SerialType, new_serial_type_def: SerialType, errors: &mut Vec<String>) {
        match (old_serial_type_def, new_serial_type_def) {
            (SerialType::Struct(old_struct), SerialType::Struct(new_struct)) => {
                self.merge_type_def_bodies(&mut old_struct.body, new_struct.body, errors)
            }
            (SerialType::Enum(old_enum), SerialType::Enum(new_enum)) => {
                self.merge_enum_type_defs(old_enum, new_enum, errors)
            }
            _ => {
                errors.push(String::from("one is a struct and the other is an enum"));
            }
        }
    }

    fn merge_type_def_bodies(&mut self, old_body: &mut SerialTypeBody, new_body: SerialTypeBody, errors: &mut Vec<String>) {
        match (old_body, new_body) {
            (SerialTypeBody::None, SerialTypeBody::None) => {
                // Nothing to do
            }
            (SerialTypeBody::Tuple(old_tuple), SerialTypeBody::Tuple(new_tuple)) => {
                self.merge_rust_type_slices(old_tuple, new_tuple, errors)
            }
            (SerialTypeBody::Fields(old_fields), SerialTypeBody::Fields(new_fields)) => {
                self.merge_fields(old_fields, new_fields, errors)
            }
            _ => {
                errors.push(String::from("different nested struct body types"));
            }
        }
    }

    fn merge_enum_type_defs(&mut self, old_enum: &mut SerialEnumType, new_enum: SerialEnumType, errors: &mut Vec<String>) {
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

    fn merge_fields(&mut self, old_fields: &mut [SerialFieldType], new_fields: Vec<SerialFieldType>, errors: &mut Vec<String>) {
        if old_fields.len() != new_fields.len() {
            errors.push(String::from("field lengths do not match"));
        } else {
            for (old_field, new_field) in old_fields.iter_mut().zip(new_fields.into_iter()) {
                self.merge_field_types(old_field, new_field, errors);
            }
        }
    }

    fn merge_field_types(&mut self, old_field: &mut SerialFieldType, new_field: SerialFieldType, errors: &mut Vec<String>) {
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
                (SerialRustType::Reference(old_ref), SerialRustType::Reference(new_ref)) => {
                    self.merge_rust_types(old_ref, *new_ref, errors);
                }
                (SerialRustType::Array { elem, length }, SerialRustType::Array { elem: new_elem, length: new_length }) => {
                    self.merge_rust_types(elem, *new_elem, errors);
                    if *length != new_length {
                        errors.push(String::from("array lengths do not match"));
                    }
                }
                (SerialRustType::Slice(old_slice), SerialRustType::Slice(new_slice)) => {
                    self.merge_rust_types(old_slice, *new_slice, errors);
                }
                (SerialRustType::Tuple(old_tuple), SerialRustType::Tuple(new_tuple)) => {
                    self.merge_rust_type_slices(old_tuple, new_tuple, errors);
                }
                (SerialRustType::Ident { name, generic_args }, SerialRustType::Ident { name: new_name, generic_args: new_generic_args }) => {
                    if name.as_str() != new_name.as_str() {
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

    /// Will only return null iff error:
    ///
    /// - this is an anonymous type (not supposed to be called here)
    /// - the type was unexpectedly-formed
    fn serialize_ident(&mut self, name: &str) -> SerialRustType {
        if name.starts_with('{') && name.ends_with('}') {
            error!("unexpceted: anonymous type name called in serialize_ident");
            return SerialRustType::unknown();
        }
        if !name.contains('<') && name.ends_with('>') {
            error!("unexpceted: type name ends with '>' but doesn't contain '<'");
            return SerialRustType::unknown();
        }

        let (name, generic_args) = if let Some(name) = name.strip_suffix('>') {
            let (base_name, generic_args_str) = name.split_once('<').unwrap();
            // filter_map will only return null on unexpectedly-formed
            let generic_args = generic_args_str
                .split_balanced(',', &ParenType::ALL_IN_RUST)
                .map(|arg| self.serialize_ident(arg.trim()))
                .collect::<Vec<_>>();
            (base_name, generic_args)
        } else {
            (name, Vec::new())
        };
        SerialRustType::Ident { name: name.to_string(), generic_args }
    }

    fn serialize_node_value(&mut self, node_value: &NodeInput, rust_type: &RustType) -> (Option<SerialValueHead>, SerialBody) {
        // TODO: Serialize inline tuple / array types if small enough (after calling this we can check by printing)
        match node_value {
            NodeInput::Hole => (None, SerialBody::None),
            NodeInput::Dep(dep) => {
                let (node_name, field_name) = match dep {
                    NodeInputDep::GraphInput { idx } => {
                        (String::from(NodeTypeName::INPUT), self.input_names[*idx].clone())
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
                let array_elem_type = rust_type.array_elem_type();
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
            NodeInput::Tuple(tuple_items) => {
                let tuple_elem_types = rust_type.tuple_elem_types();
                (None, SerialBody::Tuple(tuple_items.iter().enumerate().map(|(index, NodeInputWithLayout { input: elem, size: _, align: _ })| {
                    let tuple_elem_type = tuple_elem_types.map(|tuple_elem_types| &tuple_elem_types[index]);
                    let rust_type = tuple_elem_type.and_then(|tuple_elem_type| self.serialize_rust_type(tuple_elem_type));
                    let (value, value_children) = self.serialize_node_value(elem, tuple_elem_type.unwrap_or(&RustType::unknown()));
                    SerialTupleItem {
                        rust_type,
                        value,
                        value_children
                    }
                }).collect()))
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
            RustType::Known(known) => {
                if known.intrinsic.type_id() == TypeId::of::<i64>() {
                    let mut constant_data_bytes = [0; size_of::<i64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = i64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Integer(value)), SerialBody::None)
                } else if known.intrinsic.type_id() == TypeId::of::<f64>() {
                    let mut constant_data_bytes = [0; size_of::<f64>()];
                    constant_data_bytes.copy_from_slice(constant_data);
                    let value = f64::from_ne_bytes(constant_data_bytes);
                    (Some(SerialValueHead::Float(value)), SerialBody::None)
                } else if known.intrinsic.type_id() == TypeId::of::<String>() {
                    // Should be guaranteed utf-8 but just for safety
                    let value = String::from_utf8_lossy(constant_data).into_owned();
                    (Some(SerialValueHead::String(value)), SerialBody::None)
                } else {
                    error!("deserialized constant data is of unknown intrinsic type: {}", rust_type);
                    (None, SerialBody::None)
                }
            }
            RustType::Structural(structural) => match &structural.structure {
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
                }
                TypeStructure::CReprStruct { body } => (None, match body {
                    TypeStructBody::None => SerialBody::None,
                    TypeStructBody::Tuple(tuple_item_types) => {
                        self.serialize_constant_tuple(constant_data, tuple_item_types)
                    }
                    TypeStructBody::Fields(field_types) => {
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
                TypeStructure::Tuple { elements} => {
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

    fn serialize_constant_slice(&mut self, constant_data: &[u8], element_type: &RustType) -> SerialBody {
        let (inferred_size, inferred_align) = match (element_type.infer_size(), element_type.infer_align()) {
            (Some(size), Some(align)) => (size, align),
            _ => {
                error!("deserialized constant data is of slice type with unknown size and alignment, can't interpret it");
                return SerialBody::None
            }
        };
        let mut aligned_size = inferred_size;
        if aligned_size % inferred_align != 0 {
            aligned_size += inferred_align - aligned_size % inferred_align;
        }
        let inferred_len = constant_data.len() / aligned_size;
        self.serialize_constant_array(constant_data, element_type, inferred_len)
    }

    fn serialize_constant_compound<'a, U, V, It: Iterator<Item=(&'a RustType, U)>>(
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
                error!("deserialized constant data contains type with unknown size: {}", elem_type);
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