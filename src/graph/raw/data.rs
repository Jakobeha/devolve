use std::iter::zip;
use std::mem::{MaybeUninit, size_of};
use std::ptr::{copy_nonoverlapping, slice_from_raw_parts_mut};
use structural_reflection::{HasTypeName, infer_c_tuple_elem_offsets, infer_c_tuple_size, RustType};
use derive_more::{Display, Error};
use crate::raw::{NullRegion, IODataTypes, IODataType};

/// Dyamically-typed read-only data for a node-graph =
/// node-graph output data or compute function input data.
///
/// The data can read via type-checked or unsafe operations.
#[repr(transparent)]
pub struct LoadData(pub(in crate::graph) IOData);

/// Dynamically-typed write-only data for a node-graph =
/// node-graph input data or compute function output data.
///
/// The data can written via type-checked or unsafe operations.
#[repr(transparent)]
pub struct StoreData(pub(in crate::graph) IOData);

/// Untyped readable or writable data for a node-graph.
/// The data can manipulated via type-checked or unsafe operations.
///
/// [LoadData] and [StoreData] are read-only and write-only wrappers, respectively.
pub struct IOData {
    /// Types of the data elements
    rust_types: Vec<RustType>,
    /// Type nullabilities of the data elements.
    ///
    /// When reading, means "expected to be non-null". When writing, means "required to be non-null"
    type_nullabilities: Vec<NullRegion>,
    /// Data element offsets
    offsets: Vec<usize>,
    /// Actual data elements in one contiguous region (see `offsets` for their offsets)
    raw: Box<RawData>,
    /// Nullability of each data element. Null = uninitialized.
    raw_nullabilities: Vec<NullRegion>,
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
    pub fn type_nullabilities(&self) -> &[NullRegion] {
        self.0.type_nullabilities()
    }

    /// Nullability of each data element. Null = uninitialized.
    pub fn raw_nullabilities(&self) -> &[NullRegion] {
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
    pub fn type_nullabilities(&self) -> &[NullRegion] {
        self.0.type_nullabilities()
    }

    /// Nullability of each data element. Null = uninitialized.
    pub fn raw_nullabilities(&self) -> &[NullRegion] {
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
    /// Creates null (uninitialized) data of `Values` type and type nullability.
    ///
    /// This is safe but trying to read will panic unless all of the values are nullable.
    /// Use [LoadData::init] or [LoadData::init_with] instead to create initialized data.
    pub fn uninit<Values: IODataTypes>() -> Self where <Values::Inner as HasTypeName>::StaticId: Sized {
        IOData::new_raw_uninit(
            Values::iter_rust_types().collect(),
            Values::iter_max_null_regions().collect()
        )
    }

    /// Creates data of `Values` type and type nullability.
    /// Stores `values` into it.
    pub fn init<Values: IODataTypes>(values: Values) -> Self where <Values::Inner as HasTypeName>::StaticId: Sized {
        let (raw_data, null_regions) = values.split();
        IOData::new_raw(
            Values::iter_rust_types().collect(),
            Values::iter_max_null_regions().collect(),
            raw_data,
            null_regions
        )
    }

    /// Creates data of `Values` type and type nullability.
    /// Calls `fun` with the data write-only, then returns it read-only.
    /// Also checks that non-null data is initialized by `fun`.
    ///
    /// Somewhat contrary to the name, the data is not initialized if `fun` doesn't write to it.
    /// `init_with` implies that `fun` is doing some sort of initialization.
    pub fn init_with<Values: IODataTypes>(fun: impl FnOnce(&mut StoreData)) -> IOTypeCheckResult<Self> where <Values::Inner as HasTypeName>::StaticId: Sized {
        let mut output = IOData::uninit::<Values>();
        fun(output.write_only());
        output.check_all_value_nullabilities()?;
        Ok(output)
    }

    fn new_raw<T>(
        rust_types: Vec<RustType>,
        type_nullabilities: Vec<NullRegion>,
        raw_data: MaybeUninit<T>,
        raw_nullabilities: Vec<NullRegion>
    ) -> Self {
        assert_eq!(size_of::<T>(), infer_c_tuple_size(&rust_types), "size of actual type must equal inferred size of rust_types");
        let mut this = Self::_new_raw_uninit(rust_types, type_nullabilities, raw_nullabilities);

        unsafe {
            copy_nonoverlapping(&raw_data as *const _ as *const u8, this.raw.as_mut_ptr() as *mut u8, size_of::<T>());
        }

        this
    }

    /// Create uninitialied [IOData] with the given (known only at runtime) types and nullabilities
    pub(in crate::graph) fn new_raw_uninit(
        rust_types: Vec<RustType>,
        type_nullabilities: Vec<NullRegion>
    ) -> Self {
        debug_assert_eq!(rust_types.len(), type_nullabilities.len(), "# of types must be the same as # of null regions");
        let len = type_nullabilities.len();
        IOData::_new_raw_uninit(
            rust_types,
            type_nullabilities,
            vec![NullRegion::Null; len]
        )
    }

    fn _new_raw_uninit(
        rust_types: Vec<RustType>,
        type_nullabilities: Vec<NullRegion>,
        raw_nullabilities: Vec<NullRegion>
    ) -> Self {
        assert_eq!(rust_types.len(), type_nullabilities.len(), "# of types must be the same as # of null regions");

        let offsets = infer_c_tuple_elem_offsets(&rust_types).collect::<Vec<_>>();
        let raw = Box::new_uninit_slice(infer_c_tuple_size(&rust_types));

        IOData {
            rust_types,
            type_nullabilities,
            offsets,
            raw,
            raw_nullabilities,
        }
    }
    // endregion

    // region views and fields
    /// Write-only view of this data
    pub fn write_only(&mut self) -> &mut StoreData {
        unsafe { &mut *(self as *mut Self as *mut StoreData) }
    }

    /// Read-only view of this data
    pub fn read_only(&self) -> &LoadData {
        unsafe { &*(self as *const Self as *const LoadData) }
    }

    /// Number of data elements
    pub fn len(&self) -> usize {
        debug_assert_eq!(self.rust_types.len(), self.type_nullabilities.len(), "sanity check failed");
        self.rust_types.len()
    }

    /// Types of the data elements
    pub fn rust_types(&self) -> &[RustType] {
        &self.rust_types
    }

    /// Type nullabilities of the data elements.
    ///
    /// When loading, nullability = "expected to be non-null?",
    /// when storing, nullability = "required to store non-null?".
    pub fn type_nullabilities(&self) -> &[NullRegion] {
        &self.type_nullabilities
    }

    /// Nullability of each data element. Null = uninitialized
    pub fn raw_nullabilities(&self) -> &[NullRegion] {
        &self.raw_nullabilities
    }
    // endregion

    // region check type
    /// Checks that the elements are of the given type. *Doesn't* check value nullabilities (does check type nullabilities).
    pub fn check_all_types<Values: IODataTypes>(&self) -> IOTypeCheckResult<()> where <Values::Inner as HasTypeName>::StaticId: Sized {
        if self.len() != Values::len() {
            return Err(IOTypeCheckError::LengthMismatch {
                actual: self.len(),
                expected: Values::len()
            });
        }
        for (index, (actual_rust_type, expected_rust_type)) in zip(&self.rust_types, Values::iter_rust_types()).enumerate() {
            if actual_rust_type != &expected_rust_type {
                return Err(IOTypeCheckError::RustTypeMismatch {
                    index,
                    actual: actual_rust_type.clone(),
                    expected: expected_rust_type.clone()
                });
            }
        }
        for (index, (actual_nullability, expected_nullability)) in zip(&self.type_nullabilities, Values::iter_max_null_regions()).enumerate() {
            if actual_nullability != &expected_nullability {
                return Err(IOTypeCheckError::TypeNullabilityMismatch {
                    index,
                    actual: actual_nullability.clone(),
                    expected: expected_nullability.clone()
                });
            }
        }
        Ok(())
    }

    /// Check that actual elements are not null whose types expect them to be not null
    pub fn check_all_value_nullabilities(&self) -> IOTypeCheckResult<()> {
        for (index, (raw_nullability, type_nullability)) in zip(self.raw_nullabilities(), self.type_nullabilities()).enumerate() {
            if !raw_nullability.is_subset_of(type_nullability) {
                return Err(IOTypeCheckError::InvalidRawNullability {
                    index,
                    actual: raw_nullability.clone(),
                    expected: type_nullability.clone()
                });
            }
        }
        Ok(())
    }

    /// Checks that the element is of the given type. *Doesn't* check value nullability (does check type nullability).
    pub fn check_idx_type<Value: IODataType>(&self, idx: usize) -> IOTypeCheckResult<()> where <Value::Inner as HasTypeName>::StaticId: Sized {
        if idx >= self.len() {
            return Err(IOTypeCheckError::OutOfBounds {
                idx,
                len: self.len()
            });
        }
        if self.rust_types[idx] != Value::rust_type() {
            return Err(IOTypeCheckError::RustTypeMismatch {
                index: idx,
                actual: self.rust_types[idx].clone(),
                expected: Value::rust_type()
            });
        }
        if self.type_nullabilities[idx] != Value::max_null_region() {
            return Err(IOTypeCheckError::TypeNullabilityMismatch {
                index: idx,
                actual: self.type_nullabilities[idx].clone(),
                expected: Value::max_null_region()
            });
        }
        Ok(())
    }

    /// Check that actual element is not null if its type expects it to be not null
    pub fn check_idx_value_nullability(&self, idx: usize) -> IOTypeCheckResult<()> {
        if !self.raw_nullabilities()[idx].is_subset_of(&self.type_nullabilities()[idx]) {
            return Err(IOTypeCheckError::InvalidRawNullability {
                index: idx,
                actual: self.raw_nullabilities()[idx].clone(),
                expected: self.type_nullabilities()[idx].clone()
            });
        }
        Ok(())
    }
    // endregion

    // region access data unchecked
    /// Ref to raw data converted to `MaybeUninit` of the `Values::Inner` type, unchecked
    unsafe fn inner_data_unchecked<Inner>(&self) -> &MaybeUninit<Inner> {
        assert_eq!(size_of::<Inner>(), infer_c_tuple_size(&self.rust_types), "size of actual type must equal inferred size of rust_types");
        &*(self.raw.as_ptr() as *const MaybeUninit<Inner>)
    }

    /// Mutable ref to raw data converted to `MaybeUninit` of the `Values::Inner` type, unchecked
    unsafe fn inner_data_unchecked_mut<Inner>(&mut self) -> &mut MaybeUninit<Inner> {
        assert_eq!(size_of::<Inner>(), infer_c_tuple_size(&self.rust_types), "size of actual type must equal inferred size of rust_types");
        &mut *(self.raw.as_mut_ptr() as *mut MaybeUninit<Inner>)
    }

    /// Ref to raw data at `idx` converted to `MaybeUninit` of the `Value::Inner` type, unchecked
    unsafe fn inner_data_unchecked_idx<Inner>(&self, idx: usize) -> &MaybeUninit<Inner> {
        let (offset, size) = self.raw_offset_size(idx);
        &*(self.raw[offset..offset + size].as_ptr() as *const MaybeUninit<Inner>)
    }

    /// Mutable ref to raw data at `idx` converted to `MaybeUninit` of the `Value::Inner` type, unchecked
    unsafe fn inner_data_unchecked_idx_mut<Inner>(&mut self, idx: usize) -> &mut MaybeUninit<Inner> {
        let (offset, size) = self.raw_offset_size(idx);
        &mut *(self.raw[offset..offset + size].as_mut_ptr() as *mut MaybeUninit<Inner>)
    }

    /// Read all data elements.
    ///
    /// SAFETY: UB if `Values` is the wrong type, or an element is null whose type specifies non-null
    pub unsafe fn load_all_unchecked<Values: IODataTypes>(&self) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        let inner = *self.inner_data_unchecked::<Values::Inner>();
        Values::new(inner, self.raw_nullabilities().to_vec())
    }

    /// Write all data elements.
    ///
    /// SAFETY: UB if `Values` is the wrong type
    pub unsafe fn store_all_unchecked<Values: IODataTypes>(&mut self, values: Values) where <Values::Inner as HasTypeName>::StaticId: Sized {
        let (data, raw_nullabilities) = values.split();
        *self.inner_data_unchecked_mut::<Values::Inner>() = data;
        self.raw_nullabilities = raw_nullabilities;
    }

    /// Read data element at index.
    ///
    /// SAFETY: UB if `Value` is the wrong type, or the element is null but its type specifies non-null
    pub unsafe fn load_idx_unchecked<Value: IODataType>(&self, idx: usize) -> Value where <Value::Inner as HasTypeName>::StaticId: Sized {
        let inner = *self.inner_data_unchecked_idx::<Value::Inner>(idx);
        Value::new(inner, self.raw_nullabilities()[idx].clone())
    }

    /// Write data element at index.
    ///
    /// SAFETY: UB if `Value` is the wrong type, or the element (before writing) is null but its type specifies non-null
    pub unsafe fn store_idx_unchecked<Value: IODataType>(&mut self, idx: usize, value: Value) where <Value::Inner as HasTypeName>::StaticId: Sized {
        let (data, raw_nullability) = value.split();
        *self.inner_data_unchecked_idx_mut::<Value::Inner>(idx) = data;
        self.raw_nullabilities[idx] = raw_nullability;
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
    pub fn load_idx<Value: IODataType>(&self, idx: usize) -> IOTypeCheckResult<Value> where <Value::Inner as HasTypeName>::StaticId: Sized {
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
    // endregion

    // region access raw data
    /// Access raw data slice and nullability at the give index
    pub fn raw_data_idx(&self, idx: usize) -> (&RawData, &NullRegion) {
        let (offset, size) = self.raw_offset_size(idx);
        (&self.raw[offset..offset + size], &self.raw_nullabilities[idx])
    }

    /// Mutable access to raw data slice and nullability at the give index.
    ///
    /// SAFETY: Modifications must preserve the shape
    pub unsafe fn raw_data_idx_mut(&mut self, idx: usize) -> (&mut RawData, &mut NullRegion) {
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

    /// Offset and size of slice of the raw data at `idx`
    fn raw_offset_size(&self, idx: usize) -> (usize, usize) {
        (self.offsets[idx], self.rust_types[idx].size)
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
    pub unsafe fn iter_raw_data_mut(&mut self) -> impl Iterator<Item=(&mut RawData, &mut NullRegion)> {
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

type IOTypeCheckResult<T> = Result<T, IOTypeCheckError>;

/// When the type of `Value` or `Values` is different than the type of the [IOData]:
///
/// [IOData] is dynamically-typed. You can try to load or store values of any type,
/// but if the types dont't match you will get this error. You can also load or store `_unchecked`,
/// which skips this check but will cause UB on a type mismatch.
#[derive(Debug, Display, Error, Clone, PartialEq, Eq)]
pub enum IOTypeCheckError {
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
    #[display(fmt = "type mismatch at index {}: actual={} expected={}", index, "actual.type_name.unqualified()", "expected.type_name.unqualified()")]
    RustTypeMismatch {
        index: usize,
        actual: RustType,
        expected: RustType
    },
    #[display(fmt = "type nullability mismatch at index {}: actual={} expected={}", index, actual, expected)]
    TypeNullabilityMismatch {
        index: usize,
        actual: NullRegion,
        expected: NullRegion
    },
    #[display(fmt = "value is null when type is not nullable at index {}", index)]
    InvalidRawNullability {
        index: usize,
        actual: NullRegion,
        expected: NullRegion
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