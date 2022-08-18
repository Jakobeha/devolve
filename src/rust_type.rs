use derive_more::Display;
use std::any::{type_name, TypeId};
use std::collections::HashMap;
use std::iter::zip;
use std::sync::RwLock;
use std::mem::{align_of, size_of};
use lazy_static::lazy_static;
use log::error;
use crate::misc::catch_and_log::catch_and_log;

/// Information about a Rust type which may not be linked statically.
///
/// To check for type inference, we use nominal subtyping if we know the type,
/// and structural subtyping if we only know the structure.
#[derive(Debug, Display, Clone, PartialEq)]
pub enum RustType {
    Known(KnownRustType),
    Structural(StructuralRustType)
}

/// Information about a Rust type that we don't have statically.
/// Instead we have the type name and explicit struct members or enum variants.
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", type_name)]
pub struct StructuralRustType {
    /// Type name. Corresponds to [std::any::type_name],
    /// except in practice [std::any::type_name] returns with modules and this doesn't
    /// (although both are technically unspecified and shouldn't be relied on)
    pub type_name: String,
    /// Computed size. Corresponds to [std::size_of]
    pub size: Option<usize>,
    /// Computed alignment. Corresponds to [std::align_of]
    pub align: Option<usize>,
    /// Structure
    pub structure: TypeStructure
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeStructure {
    Opaque,
    Primitive(PrimitiveType),
    CReprEnum { variants: Vec<TypeEnumVariant> },
    CReprStruct { body: TypeStructBody },
    Pointer { referenced: Box<RustType> },
    Tuple { elements: Vec<RustType> },
    Array { elem: Box<RustType>, length: usize },
    Slice { elem: Box<RustType> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    I64,
    F64
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnumVariant {
    pub name: String,
    pub body: TypeStructBody
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeStructBody {
    None,
    Tuple(Vec<RustType>),
    Fields(Vec<TypeStructField>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStructField {
    pub name: String,
    pub rust_type: RustType
}

/// Type we know but intrinsic information and structure on
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", intrinsic)]
pub struct KnownRustType {
    pub intrinsic: IntrinsicRustType,
    pub structure: TypeStructure
}

/// Information which you can get with an arbitrary but static `T` via [IntrinsicRustType::of].
///
/// This is all information proided in intrinsic methods, but often we need all of it.
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", type_name)]
pub struct IntrinsicRustType {
    /// [std::any::TypeId::of]
    pub id: TypeId,
    /// [std::any::type_name]
    pub type_name: &'static str,
    /// [std::size_of]
    pub size: usize,
    /// [std::align_of]
    pub align: usize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IsSubtypeOf {
    No,
    Unknown,
    Yes,
}

/// A type we know the structure of at compile type. We can derive this
pub trait HasStructure: 'static {
    fn structure() -> TypeStructure;
}

lazy_static! {
    static ref KNOWN_TYPES: RwLock<HashMap<String, KnownRustType>> = RwLock::new(HashMap::new());
}

impl IntrinsicRustType {
    pub const fn of<T: 'static>() -> Self {
        IntrinsicRustType {
            id: TypeId::of::<T>(),
            type_name: type_name::<T>(),
            size: size_of::<T>(),
            align: align_of::<T>()
        }
    }
}

impl KnownRustType {
    pub fn new<T: HasStructure>() -> Self {
        KnownRustType {
            intrinsic: IntrinsicRustType::of::<T>(),
            structure: T::structure()
        }
    }

    pub fn new_opaque<T: 'static>() -> Self {
        KnownRustType {
            intrinsic: IntrinsicRustType::of::<T>(),
            structure: TypeStructure::Opaque
        }
    }

    pub fn register(known_type: KnownRustType) {
        let type_name = known_type.structural_type_name();
        if let Some(mut known_types) = catch_and_log!(KNOWN_TYPES.write(), "known rust types poisoned") {
            if let Some(existing_type) = known_types.get(&type_name) {
                if existing_type != &known_type {
                    error!("known rust type with name {} already registered, and it's a different type", type_name);
                }
            }
            known_types.insert(type_name, known_type);
        }
    }

    pub fn get(type_name: &str) -> Option<KnownRustType> {
        match catch_and_log!(KNOWN_TYPES.read(), "known rust types poisoned") {
            None => None,
            Some(known_types) => known_types.get(type_name).cloned()
        }
    }

    /// Type name which corresponds to [StructuralRustType::type_name]
    pub fn structural_type_name(&self) -> String {
        self.simple_type_name()
    }

    /// Type name without module qualifiers
    pub fn simple_type_name(&self) -> String {
        self.intrinsic.simple_type_name()
    }
}

impl IntrinsicRustType {
    const TYPE_SEPARATORS: [char; 6] = ['<', '>', '(', ')', '[', ']'];

    /// Type name which corresponds to [StructuralRustType::type_name]
    pub fn structural_type_name(&self) -> String {
        self.simple_type_name()
    }

    /// Type name without module qualifiers
    pub fn simple_type_name(&self) -> String {
        self.type_name
            .split_inclusive(&Self::TYPE_SEPARATORS)
            .flat_map(|s| if s.ends_with(&Self::TYPE_SEPARATORS) { let (a, b) = s.split_at(s.len() - 1); [a, b] } else { [s, ""] })
            .filter(|s| !s.is_empty())
            .map(|type_name| type_name.rsplit("::").next().unwrap())
            .collect::<Vec<&str>>()
            .join("")
    }
}

impl RustType {
    pub fn unknown() -> RustType {
        RustType::Structural(StructuralRustType::unknown())
    }

    pub fn structural(&self) -> StructuralRustType {
        match self {
            RustType::Structural(st) => st.clone(),
            RustType::Known(known) => StructuralRustType {
                type_name: known.structural_type_name(),
                size: Some(known.intrinsic.size),
                align: Some(known.intrinsic.align),
                structure: known.structure.clone()
            }
        }
    }
}

impl StructuralRustType {
    pub fn unknown() -> StructuralRustType {
        StructuralRustType {
            type_name: "{unknown}".to_string(),
            size: None,
            align: None,
            structure: TypeStructure::Opaque
        }
    }

    pub fn bottom() -> StructuralRustType {
        StructuralRustType {
            type_name: "{bottom}".to_string(),
            size: None,
            align: None,
            structure: TypeStructure::Opaque
        }
    }

    pub fn i64() -> StructuralRustType {
        StructuralRustType {
            type_name: "i64".to_string(),
            size: Some(size_of::<i64>()),
            align: Some(align_of::<i64>()),
            structure: TypeStructure::Primitive(PrimitiveType::I64)
        }
    }

    pub fn f64() -> StructuralRustType {
        StructuralRustType {
            type_name: "f64".to_string(),
            size: Some(size_of::<f64>()),
            align: Some(align_of::<f64>()),
            structure: TypeStructure::Primitive(PrimitiveType::F64)
        }
    }

    pub fn string() -> StructuralRustType {
        StructuralRustType {
            type_name: "String".to_string(),
            size: Some(size_of::<String>()),
            align: Some(align_of::<String>()),
            structure: TypeStructure::Opaque
        }
    }
}

impl RustType {
    /// Returns equal if both types are known, or structural subtype otherwise
    pub fn is_rough_subtype_of(&self, other: &RustType) -> IsSubtypeOf {
        match (self, other) {
            (RustType::Known(this), RustType::Known(other)) => {
                IsSubtypeOf::known(this.intrinsic.id == other.intrinsic.id)
            }
            (RustType::Structural(this), RustType::Structural(other)) => {
                this.structure.is_structural_subtype_of(&other.structure)
            }
            (RustType::Known(this), RustType::Structural(other)) => {
                this.structure.is_structural_subtype_of(&other.structure)
            }
            (RustType::Structural(this), RustType::Known(other)) => {
                other.structure.is_structural_subtype_of(&this.structure)
            }
        }
    }

    pub fn refine_from(&mut self, other: RustType) {
        match self {
            // Already fully known
            RustType::Known(_) => {}
            RustType::Structural(this) => {
                match other {
                    RustType::Structural(other) => this.structure.refine_from(other.structure),
                    RustType::Known(other) => this.structure.refine_from(other.structure),
                }
            }
        }
    }
}

impl StructuralRustType {
    pub fn is_structural_subtype_of(&self, other: &StructuralRustType) -> IsSubtypeOf {
        self.structure.is_structural_subtype_of(&other.structure)
    }

    pub fn refine_from(&mut self, other: StructuralRustType) {
        self.structure.refine_from(other.structure)
    }
}

impl TypeStructure {
    // pub const UNIT: TypeStructure = TypeStructure::Tuple { elements: Vec::new() };

    pub fn is_structural_subtype_of(&self, other: &TypeStructure) -> IsSubtypeOf {
        match (self, other) {
            (TypeStructure::Opaque, _) | (_, TypeStructure::Opaque) => IsSubtypeOf::Unknown,
            (TypeStructure::Primitive(primitive), TypeStructure::Primitive(other_primitive)) => {
                IsSubtypeOf::known(primitive == other_primitive)
            },
            (TypeStructure::CReprEnum { variants }, TypeStructure::CReprEnum { variants: other_variants }) => {
                variants.iter().map(|variant| {
                    match other_variants.iter().find(|other_variant| variant.name == other_variant.name) {
                        None => IsSubtypeOf::No,
                        Some(other_variant) => variant.body.is_structural_subtype_of(&other_variant.body)
                    }
                }).min().unwrap_or(IsSubtypeOf::Yes)
            }
            (TypeStructure::CReprStruct { body }, TypeStructure::CReprStruct { body: other_body }) => {
                body.is_structural_subtype_of(other_body)
            }
            (TypeStructure::Pointer { referenced }, TypeStructure::Pointer { referenced: other_referenced }) => {
                IsSubtypeOf::known(referenced == other_referenced)
            }
            (TypeStructure::Tuple { elements }, TypeStructure::Tuple { elements: other_elements }) => {
                if elements.len() != other_elements.len() {
                    IsSubtypeOf::No
                } else {
                    zip(elements.iter(), other_elements.iter()).map(|(element, other_element)| {
                        element.is_rough_subtype_of(other_element)
                    }).min().unwrap_or(IsSubtypeOf::Yes)
                }
            }
            (TypeStructure::Array { elem, length }, TypeStructure::Array { elem: other_elem, length: other_length }) => {
                if length != other_length {
                    IsSubtypeOf::No
                } else {
                    elem.is_rough_subtype_of(other_elem)
                }
            }
            (TypeStructure::Slice { elem }, TypeStructure::Slice { elem: other_elem }) => {
                elem.is_rough_subtype_of(other_elem)
            }
            _ => IsSubtypeOf::No
        }
    }

    /// Attempts to refine from the other type.
    ///
    /// If they are different then takes priority from the first type.
    /// Use [is_structural_subtype_of] if you want to do something else (e.g. throw error or set to opaque).
    pub fn refine_from(&mut self, other: TypeStructure) {
        // Can't put in match expr because of borrowing rules
        if matches!(self, TypeStructure::Opaque) {
            *self = other;
            return;
        }

        match (self, other) {
            (TypeStructure::Opaque, _) => unreachable!(),
            (TypeStructure::CReprEnum { variants }, TypeStructure::CReprEnum { variants: mut other_variants }) => {
                for variant in variants.iter_mut() {
                    if let Some(other_variant_idx) = other_variants.iter().position(|other_variant| variant.name == other_variant.name) {
                        let other_variant = other_variants.remove(other_variant_idx);
                        variant.body.refine_from(other_variant.body);
                    }
                }
            },
            (TypeStructure::CReprStruct { body }, TypeStructure::CReprStruct { body: other_body }) => {
                body.refine_from(other_body);
            }
            (TypeStructure::Pointer { referenced }, TypeStructure::Pointer { referenced: other_referenced }) => {
                referenced.refine_from(*other_referenced);
            }
            (TypeStructure::Tuple { elements }, TypeStructure::Tuple { elements: other_elements }) => {
                for (element, other_element) in elements.iter_mut().zip(other_elements.into_iter()) {
                    element.refine_from(other_element);
                }
            }
            (TypeStructure::Array { elem, length: _ }, TypeStructure::Array { elem: other_elem, length: _ }) => {
                elem.refine_from(*other_elem);
            }
            (TypeStructure::Slice { elem }, TypeStructure::Slice { elem: other_elem }) => {
                elem.refine_from(*other_elem);
            }
            _ => {}
        }
    }
}

impl TypeStructBody {
    fn is_structural_subtype_of(&self, other: &TypeStructBody) -> IsSubtypeOf {
        match (self, other) {
            (TypeStructBody::None, TypeStructBody::None) => IsSubtypeOf::Yes,
            (TypeStructBody::Tuple(elements), TypeStructBody::Tuple(other_elements)) => {
                if elements.len() != other_elements.len() {
                    IsSubtypeOf::No
                } else {
                    zip(elements.iter(), other_elements.iter()).map(|(element, other_element)| {
                        element.is_rough_subtype_of(other_element)
                    }).min().unwrap_or(IsSubtypeOf::Yes)
                }
            }
            (TypeStructBody::Fields(fields), TypeStructBody::Fields(other_fields)) => {
                if fields.len() < other_fields.len() {
                    IsSubtypeOf::No
                } else {
                    fields.iter().map(|field| {
                        match other_fields.iter().find(|other_field| field.name == other_field.name) {
                            None => IsSubtypeOf::No,
                            Some(other_field) => field.rust_type.is_rough_subtype_of(&other_field.rust_type)
                        }
                    }).min().unwrap_or(IsSubtypeOf::Yes)
                }
            }
            _ => IsSubtypeOf::No
        }
    }

    fn refine_from(&mut self, other: TypeStructBody) {
        match (self, other) {
            (TypeStructBody::Tuple(elements), TypeStructBody::Tuple(other_elements)) => {
                for (element, other_element) in elements.iter_mut().zip(other_elements.into_iter()) {
                    element.refine_from(other_element);
                }
            }
            (TypeStructBody::Fields(fields), TypeStructBody::Fields(mut other_fields)) => {
                for field in fields.iter_mut() {
                    if let Some(other_field_idx) = other_fields.iter().position(|other_field| field.name == other_field.name) {
                        let other_field = other_fields.remove(other_field_idx);
                        field.rust_type.refine_from(other_field.rust_type);
                    }
                }
            }
            _ => {}
        }
    }
}

impl IsSubtypeOf {
    pub fn known(x: bool) -> Self {
        if x {
            IsSubtypeOf::Yes
        } else {
            IsSubtypeOf::No
        }
    }
}

impl RustType {
    pub fn infer_size(&self) -> Option<usize> {
        match self {
            RustType::Known(known) => Some(known.intrinsic.size),
            RustType::Structural(structural) => structural.size
        }
    }

    pub fn infer_align(&self) -> Option<usize> {
        match self {
            RustType::Known(known) => Some(known.intrinsic.align),
            RustType::Structural(structural) => structural.align
        }
    }
}

impl TypeStructure {
    pub fn infer_size(&self) -> Option<usize> {
        match self {
            TypeStructure::Opaque => None,
            TypeStructure::Primitive(primitive) => Some(primitive.size()),
            TypeStructure::CReprEnum { variants } => if variants.iter().any(|variant| variant.infer_size().is_none()) {
                None
            } else {
                let discriminant_size = discriminant_size(variants.len());
                let data_size = variants.iter().map(|variant| variant.infer_size().unwrap()).max().unwrap_or(0);
                Some(discriminant_size + data_size)
            }
            TypeStructure::CReprStruct { body } => body.infer_size(),
            TypeStructure::Pointer { referenced: _ } => Some(size_of::<*const ()>()),
            TypeStructure::Tuple { elements } => infer_tuple_size(elements),
            TypeStructure::Array { elem, length } => infer_array_size(elem, *length),
            TypeStructure::Slice { elem: _ } => None
        }
    }

    pub fn infer_align(&self) -> Option<usize> {
        match self {
            TypeStructure::Opaque => None,
            TypeStructure::Primitive(primitive) => Some(primitive.align()),
            TypeStructure::CReprEnum { variants } => if variants.iter().any(|variant| variant.infer_align().is_none()) {
                None
            } else {
                let discriminant_align = discriminant_align(variants.len());
                let data_align = variants.iter().map(|variant| variant.infer_align().unwrap()).max().unwrap_or(0);
                Some(usize::max(discriminant_align, data_align))
            }
            TypeStructure::CReprStruct { body } => body.infer_align(),
            TypeStructure::Pointer { referenced: _ } => Some(align_of::<*const ()>()),
            TypeStructure::Tuple { elements } => infer_tuple_align(elements),
            TypeStructure::Array { elem, length } => infer_array_align(elem, *length),
            TypeStructure::Slice { elem: _ } => None
        }
    }
}

impl PrimitiveType {
    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::I64 => size_of::<i64>(),
            PrimitiveType::F64 => size_of::<f64>()
        }
    }

    pub fn align(&self) -> usize {
        match self {
            PrimitiveType::I64 => align_of::<i64>(),
            PrimitiveType::F64 => align_of::<f64>()
        }
    }
}

impl TypeEnumVariant {
    fn infer_size(&self) -> Option<usize> {
        self.body.infer_size()
    }

    fn infer_align(&self) -> Option<usize> {
        self.body.infer_align()
    }
}

impl TypeStructBody {
    fn infer_size(&self) -> Option<usize> {
        match self {
            TypeStructBody::None => Some(0),
            TypeStructBody::Tuple(elems) => infer_tuple_size(elems),
            TypeStructBody::Fields(fields) => infer_tuple_size(fields.iter().map(|field| &field.rust_type))
        }
    }

    fn infer_align(&self) -> Option<usize> {
        match self {
            TypeStructBody::None => Some(0),
            TypeStructBody::Tuple(elems) => infer_tuple_align(elems),
            TypeStructBody::Fields(fields) => infer_tuple_align(fields.iter().map(|field| &field.rust_type))
        }
    }
}

impl Default for TypeStructBody {
    fn default() -> Self {
        TypeStructBody::None
    }
}

// Note: technically tuples don't have a defined repr according to Rust

pub fn infer_tuple_size<'a>(elems: impl IntoIterator<Item=&'a RustType>) -> Option<usize> {
    let mut cumulative_size = 0;
    for elem in elems {
        let size = elem.infer_size()?;
        let align = elem.infer_align()?;
        if cumulative_size % align != 0 {
            cumulative_size += align - (cumulative_size % align);
        }
        cumulative_size += size;
    }
    Some(cumulative_size)
}

pub fn infer_tuple_align<'a>(elems: impl IntoIterator<Item=&'a RustType>) -> Option<usize> {
    let mut max_align = 0;
    for elem in elems {
        let align = elem.infer_align()?;
        if max_align < align {
            max_align = align;
        }
    }
    Some(max_align)
}

pub fn infer_array_size(elem: &RustType, length: usize) -> Option<usize> {
    let mut aligned_size = elem.infer_size()?;
    let align = elem.infer_align()?;
    if aligned_size % align != 0 {
        aligned_size += align - aligned_size % align;
    }
    Some(aligned_size * length)
}

pub fn infer_array_align(elem: &RustType, length: usize) -> Option<usize> {
    let mut aligned_size = elem.infer_size()?;
    let align = elem.infer_align()?;
    if aligned_size % align != 0 {
        aligned_size += align - (aligned_size % align);
    }
    Some(aligned_size * length)
}

fn discriminant_size(_num_discriminants: usize) -> usize {
    // "but it selects the same size as the C compiler would use for the given target for an equivalent C-enum declaration"
    // I have no idea if this is correct. C is defined to represent enums as ints. I know this is wrong on systems where int != 4 bytes,
    // but don't know how to detect that.
    4
}

fn discriminant_align(_num_discriminants: usize) -> usize {
    // same as above
    4
}