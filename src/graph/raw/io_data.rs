use std::iter::zip;
use std::mem::{MaybeUninit, size_of};
use std::ptr::{copy_nonoverlapping, slice_from_raw_parts_mut};
use structural_reflection::{align_up, HasTypeName, IndexPath, infer_c_tuple_elem_offsets, infer_c_tuple_size, RustType, RustTypeName};
use derive_more::{Display, Error};
use join_lazy_fmt::Join;
use structural_reflection::misc::try_index::{NotFound, TryIndex};
use crate::error::GraphIOCheckError::IOTypeCheckError;
use crate::raw::{Nullability, IODataTypes};

pub enum IODataOffsets {
    Atomic,
    Compound {
        offsets_sizes: Vec<(usize, usize)>,
        children: Vec<IODataOffsets>
    }
}

/// Dynamically-typed data for a node-graph: raw data with type/nullability info.
///
/// This is also the owned version of [IODataRead] and [IODataWrite]
pub struct IOData {
    /// Types of the data
    rust_type: RustType,
    /// Type nullability of the data.
    ///
    /// When reading, means "expected to be non-null". When writing, means "required to be non-null"
    type_nullability: Nullability,
    /// Whether the data is atomic or compound, and offsets of sub-values
    offsets: IODataOffsets,
    /// Actual nullability of the data.
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    raw_nullability: Nullability,
    /// Raw data of the value
    raw: Box<RawData>,
}

/// Dyamically-typed read-only data for a node-graph (raw data with type/nullability info) =
/// node-graph output data or compute function input data.
///
/// The data can read via type-checked or unsafe operations.
///
/// Sub-regions of data are also [IODataRead]
pub struct IODataRead<'a> {
    /// Type of the data
    rust_type: &'a RustType,
    /// Type nullability of the data
    ///
    /// When reading, means "expected to be non-null". When writing, means "required to be non-null"
    type_nullability: &'a Nullability,
    /// Whether the data is atomic or compound, and offsets of sub-values
    offsets: &'a IODataOffsets,
    /// Actual nullability of the data
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    raw_nullability: &'a Nullability,
    /// Raw data of the value
    raw: &'a RawData
}

/// Dynamically-typed write-only data for a node-graph (raw data with type/nullability info) =
/// node-graph input data or compute function output data.
///
/// The data can written via type-checked or unsafe operations.
///
/// Sub-regions of data are also [IODataWrite]
pub struct IODataWrite<'a> {
    /// Type of the data
    rust_type: &'a RustType,
    /// Type nullability of the data
    ///
    /// When reading, means "expected to be non-null". When writing, means "required to be non-null"
    type_nullability: &'a Nullability,
    /// Whether the data is atomic or compound, and offsets of sub-values
    offsets: &'a IODataOffsets,
    /// Actual nullability of the data
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    raw_nullability: &'a mut Nullability,
    /// Raw data of the value
    raw: &'a mut RawData
}

/// Dynamic type info which allows you to check before performing an operation on [IODataRead] or [IODataWrite].
pub struct IODataCheck<'a> {
    /// Type of the data
    rust_type: &'a RustType,
    /// Type nullability of the data
    ///
    /// When reading, means "expected to be non-null". When writing, means "required to be non-null"
    type_nullability: &'a Nullability,

    /// Actual nullability of the data
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    raw_nullability: &'a Nullability,
}

pub type RawData = [MaybeUninit<u8>];

impl LoadData {
    /// Creates data of `Values` type and type nullability.
    /// Stores `values` into it.
    pub fn init<Values: IODataTypes>(values: Values) -> Self where <Values::Inner as HasTypeName>::StaticId: Sized {
        LoadData(IOData::init(values))
    }

    /// Creates data of `Values` type and type nullability.
    /// Calls `fun` with the data write-only, then returns it read-only.
    /// Also checks that non-null data is initialized by `fun`.
    ///
    /// Somewhat contrary to the name, the data is not initialized if `fun` doesn't write to it.
    /// `init_with` implies that `fun` is doing some sort of initialization.
    pub fn init_with<Values: IODataTypes>(fun: impl FnOnce(&mut StoreData)) -> IOTypeCheckResult<Self> where <Values::Inner as HasTypeName>::StaticId: Sized {
        Ok(LoadData(IOData::init_with::<Values>(fun)?))
    }

    /// Number of elements
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Types of the data elements
    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    /// Type nullabilities of the data elements = "expected to be non-null?"
    pub fn type_nullabilities(&self) -> &[Nullability] {
        self.0.type_nullabilities()
    }

    /// Nullability of each data element. Null = uninitialized.
    pub fn raw_nullabilities(&self) -> &[Nullability] {
        self.0.raw_nullabilities()
    }

    /// Returns `Err` iff `Value` is the wrong type
    pub fn check_idx_type<Value: IODataType>(&self, idx: usize) -> IOTypeCheckResult<()> where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.check_idx_type::<Value>(idx)
    }

    /// Get actual data element at the given index.
    ///
    /// SAFETY: UB if `Value` is the wrong type, or value is null when type is expected to be non-null.
    pub unsafe fn load_idx_unchecked<Value: IODataType>(&self, idx: usize) -> Value where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.load_idx_unchecked(idx)
    }

    /// Get actual data element at the given index.
    ///
    /// Returns `Err` if `Value` is the wrong type, or value is null when type is expected to be non-null.
    pub fn load_idx<Value: IODataType>(&self, idx: usize) -> IOTypeCheckResult<Value> where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.load_idx(idx)
    }

    /// Returns `Err` if `Values` is the wrong type.
    pub fn check_all_types<Values: IODataTypes>(&self) -> IOTypeCheckResult<()> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.check_all_types::<Values>()
    }

    /// Get actual data elements.
    ///
    /// SAFETY: UB if `Values` is the wrong type, or any element is null when its type is expected to be non-null.
    pub unsafe fn load_all_unchecked<Values: IODataTypes>(&self) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.load_all_unchecked()
    }

    /// Get actual data elements.
    ///
    /// Returns `Err` iff `Values` is the wrong type, or any element is null when its type is expected to be non-null.
    pub fn load_all<Values: IODataTypes>(&self) -> IOTypeCheckResult<Values> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.load_all()
    }
}

impl StoreData {
    /// Creates null (uninitialized) readable data of `Values` type and type nullability.
    ///
    /// This is safe but trying to read will panic unless all of the values are nullable.
    /// Use [LoadData::init] or [LoadData::init_with] instead to create initialized data.
    pub fn uninit<Values: IODataTypes>() -> Self where <Values::Inner as HasTypeName>::StaticId: Sized {
        StoreData(IOData::uninit::<Values>())
    }

    /// Creates data of `Values` type and type nullability.
    /// Stores `values` into it.
    pub fn init<Values: IODataTypes>(values: Values) -> Self where <Values::Inner as HasTypeName>::StaticId: Sized {
        StoreData(IOData::init(values))
    }

    /// Allocates [IOData], calls `fun` with the data write-only, then loads it.
    /// *Doesn't* check that the data is initialized
    ///
    /// SAFETY: UB if non-null data is not initialized by `fun`.
    /// If `fun` doesn't write to the data but it is all nullable than there is no UB.
    pub unsafe fn with_unchecked<Values: IODataTypes>(fun: impl FnOnce(&mut StoreData)) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        let mut output = IOData::uninit::<Values>();
        fun(output.write_only());
        output.load_all_unchecked()
    }

    /// Allocates [IOData], calls `fun` with the data write-only, checks that the data is initialized, then loads it.
    /// Also checks that non-null data is initialized by `fun`.
    ///
    /// Note that the data is not initialized if `fun` doesn't write to it,
    /// and if the data is all nullable than it will not error.
    pub fn with<Values: IODataTypes>(fun: impl FnOnce(&mut StoreData)) -> IOTypeCheckResult<Values> where <Values::Inner as HasTypeName>::StaticId: Sized {
        let mut output = IOData::uninit::<Values>();
        fun(output.write_only());
        output.check_all_value_nullabilities()?;
        // SAFETY: If fun tries to write data of the wrong type,
        // it will panic (if checked) or cause UB in a prior unsafe block (if unchecked)
        Ok(unsafe { output.load_all_unchecked() })
    }

    /// Number of elements
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Types of the data elements
    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    /// Type nullabilities of the data elements = "expected to be non-null?"
    pub fn type_nullabilities(&self) -> &[Nullability] {
        self.0.type_nullabilities()
    }

    /// Nullability of each data element. Null = uninitialized.
    pub fn raw_nullabilities(&self) -> &[Nullability] {
        self.0.raw_nullabilities()
    }

    /// Returns `Err` iff `Value` is the wrong type
    pub fn check_idx_type<Value: IODataType>(&self, idx: usize) -> IOTypeCheckResult<()> where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.check_idx_type::<Value>(idx)
    }

    /// Get actual data element at the given index.
    ///
    /// SAFETY: UB if `Value` is the wrong type, or value is null when type is expected to be non-null.
    pub unsafe fn store_idx_unchecked<Value: IODataType>(&mut self, idx: usize, value: Value) where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.store_idx_unchecked(idx, value)
    }

    /// Get actual data element at the given index.
    ///
    /// Returns `Err` if `Value` is the wrong type, or value is null when type is expected to be non-null.
    pub fn store_idx<Value: IODataType>(&mut self, idx: usize, value: Value) -> IOTypeCheckResult<()> where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.0.store_idx(idx, value)
    }

    /// Returns `Err` iff `Values` is the wrong type
    pub fn check_all_types<Values: IODataTypes>(&self) -> IOTypeCheckResult<()> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.check_all_types::<Values>()
    }

    /// Get actual data elements.
    ///
    /// SAFETY: UB if `Values` is the wrong type
    pub unsafe fn store_all_unchecked<Values: IODataTypes>(&mut self, values: Values) where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.store_all_unchecked(values)
    }

    /// Get actual data elements.
    ///
    /// Returns `Err` iff `Values` is the wrong type
    pub fn store_all<Values: IODataTypes>(&mut self, values: Values) -> IOTypeCheckResult<()> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.0.store_all(values)
    }
}

impl IOData {
    // region create
    /// Creates null (uninitialized) data of `Value` type and type nullability.
    ///
    /// This is safe but trying to read will panic unless the value is nullable.
    /// Use [LoadData::init] or [LoadData::init_with] instead to create initialized data.
    pub fn uninit<Value: IODataType>() -> Self {
        IOData::uninit_dynamic(
            Value::rust_type(),
            Value::type_nullability()
        )
    }

    /// Creates data of `Value` type and type nullability.
    /// Stores `value` into it.
    pub fn init<Value: IODataType>(values: Value) -> Self {
        let mut this = IOData::uninit::<Value>();
        unsafe { this.as_mut().store_unchecked(values) };
        this
    }

    /// Creates data of `Value` type and type nullability.
    /// Calls `fun` with the data write-only, then returns it read-only.
    /// Also checks that non-null data is initialized by `fun`.
    ///
    /// Somewhat contrary to the name, the data is not initialized if `fun` doesn't write to it.
    /// `init_with` implies that `fun` is doing some sort of initialization.
    pub fn init_with<Value: IODataType>(fun: impl FnOnce(&mut StoreData)) -> IOTypeCheckResult<Self> {
        let mut output = IOData::uninit::<Value>();
        fun(output.write_only());
        output.check_raw_nullability()?;
        Ok(output)
    }

    /// Create uninitialied [IOData] with the given (known only at runtime) type and type nullability
    pub fn uninit_dynamic(
        rust_type: RustType,
        type_nullability: Nullability
    ) -> Self {
        let offsets = IODataOffsets::of(&rust_type);
        let raw = Box::new_uninit_slice(rust_type.size);

        IOData {
            rust_type,
            type_nullability,
            offsets,
            raw_nullability: Nullability::Null,
            raw,
        }
    }
    // endregion

    // region views and fields
    /// As [IODataRead]
    pub fn as_read(&self) -> IODataRead<'_> {
        IODataRead {
            rust_type: &self.rust_type,
            type_nullability: &self.type_nullability,
            offsets: &self.offsets,
            raw_nullability: &self.raw_nullability,
            raw: &self.raw
        }
    }

    /// As [IODataWrite]
    pub fn as_write(&mut self) -> IODataWrite<'_> {
        IODataWrite {
            rust_type: &self.rust_type,
            type_nullability: &self.type_nullability,
            offsets: &self.offsets,
            raw_nullability: &mut self.raw_nullability,
            raw: &mut self.raw
        }
    }

    /// As [IODataCheck]
    pub fn as_check(&self) -> IODataCheck<'_> {
        IODataCheck {
            rust_type: &self.rust_type,
            type_nullability: &self.type_nullability,
            raw_nullability: &self.raw_nullability,
        }
    }
    // endregion
}

// region check type
impl<'a> IODataCheck<'a> {
    /// Checks that the element is of the given type
    pub fn check_type<Value: HasTypeName>(&self) -> IOTypeCheckResult<()> {
        if &self.rust_type.type_name != &Value::type_name() {
            return Err(IOTypeCheckError::root(IOTypeCheckErrorCause::TypeMismatch {
                actual: self.rust_type.type_name.clone(),
                expected: Value::type_name()
            }));
        }
        Ok(())
    }

    /// Check that actual element is not null if its type expects it to be not null
    pub fn check_value_nullability(&self) -> IOTypeCheckResult<()> {
        if !self.raw_nullability.is_subset_of(&self.type_nullability) {
            return Err(IOTypeCheckError::root(IOTypeCheckErrorCause::RawNullabilityInvalid {
                actual: self.raw_nullability.clone(),
                expected: self.type_nullability.clone()
            }));
        }
        Ok(())
    }
}

impl<'a> IODataRead<'a> {
    fn as_check(&self) -> IODataCheck<'a> {
        IODataCheck {
            rust_type: self.rust_type,
            type_nullability: self.type_nullability,
            raw_nullability: self.raw_nullability
        }
    }
}

impl<'a> IODataWrite<'a> {
    fn as_check(&self) -> IODataCheck<'a> {
        IODataCheck {
            rust_type: self.rust_type,
            type_nullability: self.type_nullability,
            raw_nullability: self.raw_nullability
        }
    }
}
// endregion

// region access data unchecked
impl IODataOffsets {
    /// Offset and size of slice of the raw data at `idx_path`
    fn raw_offset_size_at_path(&self, idx_path: &[usize]) -> (usize, usize) {
        let result = &self[idx_path];
        (result.offset, result.size)
    }
            let base_idx = idx_path[0];
            let mut offset = self[base_idx];
            let mut rust_type = &self.rust_types[base_idx];
            for &sub_idx in &idx_path[1..] {
                let mut subtypes = rust_type.structure
                    .general_compound_elem_types().expect("raw_offset_size_at_path: bad rust type (type ended before index path)");
                for _ in 0..sub_idx {
                    let prev_type = subtypes.next().expect("raw_offset_size_at_path: bad rust type (index path index out of type's bounds)");
                    offset = align_up(offset, prev_type.align);
                    offset += prev_type.size;
                }
                rust_type = subtypes.next().expect("raw_offset_size_at_path: bad rust type (index path index out of type's bounds)");
                offset = align_up(offset, rust_type.align);
            }
            (offset, rust_type.size)
        }
    }
}

impl<'a> IODataRead<'a> {
    /// Ref to raw data interpreted as `MaybeUninit<Value>`, unchecked
    unsafe fn coerce_raw<Value>(&self) -> &MaybeUninit<Value> {
        &*(self.raw.as_ptr() as *const MaybeUninit<Value>)
    }

    /// Ref to raw data at `idx_path` interpreted as `MaybeUninit<Value>`, unchecked
    unsafe fn coerce_raw_idx_path<Inner>(&self, idx_path: &[usize]) -> &MaybeUninit<Inner> {
        let (offset, size) = self.as_check().raw_offset_size_of_path(idx_path);
        &*(self.raw[offset..offset + size].as_ptr() as *const MaybeUninit<Inner>)
    }
}

impl<'a> IODataWrite<'a> {
    /// Mutable ref to raw data interpreted as `MaybeUninit<Value>`, unchecked
    unsafe fn coerce_raw_mut<Value>(&mut self) -> &mut MaybeUninit<Value> {
        &mut *(self.raw.as_mut_ptr() as *mut MaybeUninit<Value>)
    }

    /// Mutable ref to raw data at `idx_path` interpreted as `MaybeUninit<Value>`, unchecked
    unsafe fn coerce_raw_idx_path_mut<Inner>(&self, idx_path: &[usize]) -> &MaybeUninit<Inner> {
        let (offset, size) = self.as_check().raw_offset_size_of_path(idx_path);
        &mut *(self.raw[offset..offset + size].as_mut_ptr() as *mut MaybeUninit<Inner>)
    }
}

    /// Mutable ref to raw data interpreted as `MaybeUninit` of the `Values::Inner` type, unchecked
    unsafe fn coerce_raw_mut<Inner>(&mut self) -> &mut MaybeUninit<Inner> {
        assert_eq!(size_of::<Inner>(), infer_c_tuple_size(&self.rust_types), "size of actual type must equal inferred size of rust_types");
        &mut *(self.raw.as_mut_ptr() as *mut MaybeUninit<Inner>)
    }

    /// Ref to raw data at `idx` interpreted as `MaybeUninit` of the `Value::Inner` type, unchecked
    unsafe fn coerce_raw_idx<Inner>(&self, idx: usize) -> &MaybeUninit<Inner> {
        let (offset, size) = self.raw_offset_size(idx);
        &*(self.raw[offset..offset + size].as_ptr() as *const MaybeUninit<Inner>)
    }

    /// Mutable ref to raw data at `idx` interpreted as `MaybeUninit` of the `Value::Inner` type, unchecked
    unsafe fn coerce_raw_idx_mut<Inner>(&mut self, idx: usize) -> &mut MaybeUninit<Inner> {
        let (offset, size) = self.raw_offset_size(idx);
        &mut *(self.raw[offset..offset + size].as_mut_ptr() as *mut MaybeUninit<Inner>)
    }

    /// Mutable ref to raw data at `idx_path` interpreted as `MaybeUninit` of the `Value::Inner` type, unchecked
    unsafe fn coerce_raw_idx_path_mut<Inner>(&mut self, idx_path: &[usize]) -> &mut MaybeUninit<Inner> {
        match idx_path.len() {
            0 => self.coerce_raw_mut::<Inner>(),
            // Unnecessary but faster (?) and clean
            1 => self.coerce_raw_idx_mut::<Inner>(idx),
            _ => {
                let (offset, size) = self.raw_offset_size_of_path(idx_path);
                &mut *(self.raw[offset..offset + size].as_mut_ptr() as *mut MaybeUninit<Inner>)
            }
        }
    }

    /// Read entire data.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_all_unchecked<Value>(&self) -> Option<&Value> {
        if self.raw_nullabilities.iter().all(|nullability| nullability.is_non_null()) {
            Some(self.coerce_raw::<Value>().assume_init_ref())
        } else {
            None
        }
    }

    /// Read entire data, with a mutable reference to write data.
    /// Use `store_idx_mut` to store a value if null
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_all_mut_unchecked<Value>(&mut self) -> Option<&mut Value> {
        if self.raw_nullabilities.iter().all(|nullability| nullability.is_non_null()) {
            Some(self.coerce_raw_mut::<Value>().assume_init_mut())
        } else {
            None
        }
    }

    /// Write entire data.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn store_all_unchecked<Value>(&mut self, value: Option<Value>) {
        for nullability in &mut self.raw_nullabilities {
            *nullability = Nullability::null_iff(value.is_none());
        }
        if let Some(value) = value {
            self.coerce_raw_mut::<Value>().write(value);
        }
    }

    /// Read data element at index.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_idx_unchecked<Value>(&self, idx: usize) -> Option<&Value> {
        if self.raw_nullabilities[idx].is_non_null() {
            Some(self.coerce_raw_idx::<Value>(idx).assume_init_ref())
        } else {
            None
        }
    }

    /// Read data element at index, with a mutable reference to write data.
    /// Use `store_idx_mut` to store a value if null
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_idx_mut_unchecked<Value>(&mut self, idx: usize) -> Option<&mut Value> {
        if self.raw_nullabilities[idx].is_non_null() {
            Some(self.coerce_raw_idx_mut::<Value>(idx).assume_init_mut())
        } else {
            None
        }
    }

    /// Write data element at index.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn store_idx_unchecked<Value>(&mut self, idx: usize, value: Option<Value>) {
        self.raw_nullabilities[idx] = Nullability::null_iff(value.is_none());
        if let Some(value) = value {
            self.coerce_raw_idx_mut::<Value>(idx).write(value);
        }
    }

    /// Read data element at index path.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_path_unchecked<Value>(&self, idx_path: &[usize]) -> Option<&Value> {
        match idx_path.len() {
            0 => self.load_all_unchecked::<Value>(),
            // Unnecessary but faster (?) and clean
            1 => self.load_idx_unchecked::<Value>(idx_path[0]),
            _ => if self.raw_nullabilities[idx_path[0]][&idx_path[1..]].is_non_null() {
                Some(self.coerce_raw_path::<Value>(idx).assume_init_ref())
            } else {
                None
            }
        }
    }

    /// Read data element at index path, with a mutable reference to write data.
    /// Use `store_idx_mut` to store a value if null
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn load_path_mut_unchecked<Value>(&mut self, idx_path: &[usize]) -> Option<&mut Value> {
        match idx_path.len() {
            0 => self.load_all_mut_unchecked::<Value>(),
            // Unnecessary but faster (?) and clean
            1 => self.load_idx_mut_unchecked::<Value>(idx_path[0]),
            _ => if self.raw_nullabilities[idx_path[0]][&idx_path[1..]].is_non_null() {
                Some(self.coerce_raw_path_mut::<Value>(idx).assume_init_ref())
            } else {
                None
            }
        }
    }

    /// Write data element at index path.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn store_path_unchecked<Value>(&mut self, idx_path: &[usize], value: Option<Value>) {
        match idx_path.len() {
            0 => self.store_all_unchecked::<Value>(value),
            // Unnecessary but faster (?) and clean
            1 => self.store_idx_unchecked::<Value>(idx_path[0], value),
            _ => {
                let base_idx = idx_path[0];
                let mut nullability = &mut self.raw_nullabilities[base_idx];
                let mut rust_type = &self.rust_types[base_idx]
                for &sub_idx in &idx_path[1..] {
                    let len = rust_type.structure.general_compound_length().expect("store_path_unchecked called on wrong type (index path ended early)");
                    nullability = &mut nullability.subdivide_mut(len)[sub_idx];
                    rust_type = rust_type.structure.general_compound_elem_types().unwrap().skip(sub_idx).next().unwrap();
                }
                *nullability = Nullability::null_iff(value.is_none());

                if let Some(value) = value {
                    self.coerce_raw_path_mut::<Value>(idx).write(value);
                }
            }
        }
    }
    // endregion

    // region check and access data
    /// Read all data elements.
    ///
    /// Returns `Err` if `Values` is the wrong type, or a element is null whose type specifies non-null
    pub fn load_all<Values: IODataTypes>(&self) -> IOTypeCheckResult<Values> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.check_all_types::<Values>()?;
        self.check_all_value_nullabilities()?;
        Ok(unsafe { self.load_all_unchecked() })
    }

    /// Write all data elements.
    ///
    /// Returns `Err` if `Values` is the wrong type
    pub fn store_all<Values: IODataTypes>(&mut self, values: Values) -> IOTypeCheckResult<()> where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.check_all_types::<Values>()?;
        Ok(unsafe { self.store_all_unchecked(values) })
    }

    /// Read data element at index.
    ///
    /// Returns `Err` if `Value` is the wrong type, or the element is null but its type specifies non-null
    pub fn load_idx<Value: HasTypeName>(&self, idx: usize) -> IOTypeCheckResult<Value> {
        self.check_idx_type::<Value>(idx)?;
        self.check_idx_value_nullability(idx)?;
        Ok(unsafe { self.load_idx_unchecked(idx) })
    }

    /// Write data element at index
    ///
    /// Returns `Err` if `Value` is the wrong type, or the element (before writing) is null but its type specifies non-null
    pub fn store_idx<Value: IODataType>(&mut self, idx: usize, value: Value) -> IOTypeCheckResult<()> where <Value::Inner as HasTypeName>::StaticId: Sized {
        self.check_idx_type::<Value>(idx)?;
        Ok(unsafe { self.store_idx_unchecked(idx, value) })
    }

    /// Read data element at index path:
    /// - If path empty = loads all data
    /// - If path has one element = `load_idx`
    /// - If path has many elements = `load_idx` and then load tail
    ///
    /// Returns `Err` if `Value` is the wrong type, or the element is null but its type specifies non-null
    pub fn load_idx_path<Value: HasTypeName>(&self, idx_path: &[usize]) -> IOTypeCheckResult<Value> {
        self.check_idx_path_type::<Value>(idx)?;
        self.check_idx_path_value_nullability(idx)?;
        Ok(unsafe { self.load_idx_path_unchecked(idx) })
    }
    // endregion

    // region access raw data
    /// Access raw data slice and nullability at the give index
    pub fn raw_data_idx(&self, idx: usize) -> (&RawData, &Nullability) {
        let (offset, size) = self.raw_offset_size(idx);
        (&self.raw[offset..offset + size], &self.raw_nullabilities[idx])
    }

    /// Mutable access to raw data slice and nullability at the give index.
    ///
    /// SAFETY: Modifications must preserve the shape
    pub unsafe fn raw_data_idx_mut(&mut self, idx: usize) -> (&mut RawData, &mut Nullability) {
        let (offset, size) = self.raw_offset_size(idx);
        (&mut self.raw[offset..offset + size], &mut self.raw_nullabilities[idx])
    }

    /// Mutable pointer access to raw data slice (no nullability) at the give index.
    /// Has `self` type unwrapped because we need to simulate partial borrow.
    ///
    /// SAFETY: Modifications must preserve the shape
    unsafe fn raw_data_mut_ptr(rust_types: &[RustType], offsets: &[usize], raw: *mut MaybeUninit<u8>, idx: usize) -> *mut RawData {
        let (offset, size) = Self::raw_offset_size_ptr(rust_types, offsets, idx);
        slice_from_raw_parts_mut(raw.add(offset), size)
    }

    /// Offset and size of slice of the raw data at `idx`.
    ///
    /// Has `self` type unwrapped because we need to simulate partial borrow.
    fn raw_offset_size_ptr(rust_types: &[RustType], offsets: &[usize], idx: usize) -> (usize, usize) {
        (offsets[idx], rust_types[idx].size)
    }

    /// Mutable access to raw data slice and nullability at each index.
    ///
    /// SAFETY: Modifications must preserve the shape
    pub unsafe fn iter_raw_data_mut(&mut self) -> impl Iterator<Item=(&mut RawData, &mut Nullability)> {
        let rust_types = &self.rust_types;
        let offsets = &self.offsets;
        let raw = self.raw.as_mut_ptr();
        let raw_data_iter = (0..self.len()).map(move |idx| unsafe { &mut *Self::raw_data_mut_ptr(rust_types, offsets, raw, idx) });
        zip(raw_data_iter, self.raw_nullabilities.iter_mut())
    }

    /// Copy raw data into the other [IOData].
    ///
    /// SAFETY: UB if the types or type nullabilities are different.
    pub unsafe fn copy_data_into_unchecked(&self, dest: &mut Self) {
        debug_assert_eq!(self.raw.len(), dest.raw.len());
        debug_assert_eq!(self.raw_nullabilities.len(), dest.raw_nullabilities.len());
        unsafe { copy_nonoverlapping(self.raw.as_ptr(), dest.raw.as_mut_ptr(), self.raw.len()); }
        self.raw_nullabilities.iter().zip(dest.raw_nullabilities.iter_mut()).for_each(|(src, dest)| src.clone_into(dest));
    }
    // endregion
}

/// When the type of `Value` is different than the type of the [IOData]:
///
/// [IOData] is dynamically-typed. You can try to load or store values of any type,
/// but if the types dont't match you will get this error. You can also load or store `_unchecked`,
/// which skips this check but will cause UB on a type mismatch.
#[derive(Debug, Display, Error, Clone, PartialEq, Eq)]
#[display(fmt = "at root.{}: {}", "\".\".join(index_path)", cause)]
pub struct IOTypeCheckError<'a> {
    /// Index path of the error (if nested, [] if root)
    index_path: &'a [usize],
    /// Underlying cause of the error: `cause` happened at `index_path`.
    cause: IOTypeCheckErrorCause
}

/// Underlying cause of the error: [IOTypeCheckError] is this with an index path.
#[derive(Debug, Display, Error, Clone, PartialEq, Eq)]
pub enum IOTypeCheckErrorCause {
    #[display(fmt = "out of bounds: idx={} len={}", idx, len)]
    OutOfBounds {
        idx: usize,
        len: usize
    },
    #[display(fmt = "length mismatch: actual={} expected={}", actual, expected)]
    LengthMismatch {
        actual: usize,
        expected: usize
    },
    #[display(fmt = "type mismatch at index {}: actual={} expected={}", index, "actual.unqualified()", "expected.unqualified()")]
    TypeMismatch {
        actual: TypeName,
        expected: TypeName
    },
    #[display(fmt = "type nullability mismatch at index {}: actual={} expected={}", index, actual, expected)]
    TypeNullabilityMismatch {
        actual: Nullability,
        expected: Nullability
    },
    #[display(fmt = "value is null when type is not nullable at index {}", index)]
    RawNullabilityInvalid {
        actual: Nullability,
        expected: Nullability
    }
}

fn types_match(actual_type: &RustType, expected_type: &RustType) -> bool {
    if actual_type == expected_type {
        true
    } else {
        match (&actual_type.type_name, &expected_type.type_name) {
            (RustTypeName::Slice { elem: actual_elem }, RustTypeName::Array { elem: expected_elem, length: _ }) => {
                actual_elem == expected_elem
            }
            _ => false
        }
    }
}

#[cfg(test)]
mod tests {
    use structural_reflection::c_tuple::{c_tuple, CTuple};
    use crate::raw::{LoadData, NonNull, Nullable};

    #[test]
    pub fn regression_test_io_data() {
        let input_data = c_tuple!(
            Nullable::Some("Placeholder"),
            NonNull("Text"),
            Nullable::<bool>::None,
            Nullable::<()>::None
        );
        let inputs = LoadData::init(input_data);
        assert_eq!(inputs.load_all::<CTuple!(
            Nullable<&str>,
            NonNull<&str>,
            Nullable<bool>,
            Nullable<()>
        )>(), Ok(input_data));
    }
}