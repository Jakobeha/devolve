use std::iter::zip;
use std::mem::{MaybeUninit, size_of};
use std::ops::{Index, IndexMut};
use std::ptr::{copy_nonoverlapping, slice_from_raw_parts_mut};
use structural_reflection::{align_up, HasTypeName, IndexPath, infer_c_tuple_elem_offsets, infer_c_tuple_size, RustType, RustTypeName};
use derive_more::{Display, Error};
use join_lazy_fmt::Join;
use structural_reflection::misc::try_index::{NotFound, TryIndex};
use crate::error::GraphIOCheckError;
use crate::raw::{Nullability, IODataTypes, IODataType};

pub type RawData = [MaybeUninit<u8>];

pub struct IODataLayout {
    offset: usize,
    size: usize,
    children: Vec<IODataLayout>
}

// region io-data types
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
    offsets: IODataLayout,
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
    offsets: &'a IODataLayout,
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
    offsets: &'a IODataLayout,
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
// endregion

// region create
impl IOData {
    /// Creates null (uninitialized) data of `Value` type and type nullability.
    ///
    /// This is safe but trying to read will panic unless the value is nullable.
    /// Use [LoadData::init] or [LoadData::init_with] instead to create initialized data.
    pub fn uninit<Value: IODataType>() -> Self {
        IOData::uninit_dynamic(
            Value::rust_type(),
            Value::nullability()
        )
    }

    /// Creates data of `Value` type and type nullability.
    /// Stores `value` into it.
    pub fn init<Value: IODataType>(value: Value) -> Self {
        let mut this = IOData::uninit::<Value>();
        unsafe { this.as_write().write_unchecked(value) };
        this
    }

    /// Creates data of `Value` type and type nullability.
    /// Calls `fun` with the data write-only, then returns it read-only.
    /// Also checks that non-null data is initialized by `fun`.
    ///
    /// Somewhat contrary to the name, the data is not initialized if `fun` doesn't write to it.
    /// `init_with` implies that `fun` is doing some sort of initialization.
    pub fn init_with<Value: IODataType>(fun: impl FnOnce(&mut IODataWrite<'_>)) -> IOTypeCheckResult<Self> {
        let mut output = IOData::uninit::<Value>();
        fun(&mut output.as_write());
        output.as_check().check_value_nullability()?;
        Ok(output)
    }

    /// Create uninitialied [IOData] with the given (known only at runtime) type and type nullability
    pub fn uninit_dynamic(
        rust_type: RustType,
        type_nullability: Nullability
    ) -> Self {
        let offsets = IODataLayout::of(&rust_type);
        let raw = Box::new_uninit_slice(rust_type.size);

        IOData {
            rust_type,
            type_nullability,
            offsets,
            raw_nullability: Nullability::Null,
            raw,
        }
    }
}
// endregion

// region views and fields
impl IOData {
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
}

impl<'a> IODataRead<'a> {
    pub fn as_check(&self) -> IODataCheck<'a> {
        IODataCheck {
            rust_type: self.rust_type,
            type_nullability: self.type_nullability,
            raw_nullability: self.raw_nullability
        }
    }
}

impl<'a> IODataWrite<'a> {
    pub fn as_check(&self) -> IODataCheck<'a> {
        IODataCheck {
            rust_type: self.rust_type,
            type_nullability: self.type_nullability,
            raw_nullability: self.raw_nullability
        }
    }
}
// endregion

// region check type and nullability
impl<'a> IODataCheck<'a> {
    /// Checks that the element is of the given type
    pub fn check_type<Value: IODataType>(&self) -> IOTypeCheckResult<()> {
        if &self.rust_type.type_name != &Value::rust_type().type_name {
            return Err(IOTypeCheckError::root(IOTypeCheckErrorCause::TypeMismatch {
                actual: self.rust_type.type_name.clone(),
                expected: Value::rust_type().type_name
            }));
        }
        if self.type_nullability != Value::type_nullability() {
            return Err(IOTypeCheckError::root(IOTypeCheckErrorCause::TypeNullabilityMismatch {
                actual: self.type_nullability.clone(),
                expected: Value::type_nullability()
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
// endregion

// region index into layout
impl IODataLayout {
    fn slice(&self, data: &RawData) -> &RawData {
        &data[self.offset..self.offset + self.size]
    }

    fn slice_mut(&self, data: &mut RawData) -> &mut RawData {
        &mut data[self.offset..self.offset + self.size]
    }
}

impl<'a> Index<&'a [usize]> for IODataLayout {
    type Output = IODataLayout;

    /// Offset and size of slice of the raw data at `idx_path`
    fn index(&self, idx_path: &'a [usize]) -> &Self::Output {
        let mut output = self;
        for &sub_idx in idx_path {
            if self.children.len() == 0 {
                panic!("Index path too long")
            }
            output = &children[sub_idx];
        }
        output
    }
}

impl<'a> IndexMut<&'a [usize]> for IODataLayout {
    /// Offset and size of slice of the raw data at `idx_path`
    fn index_mut(&mut self, idx_path: &'a [usize]) -> &mut Self::Output {
        let mut output = self;
        for &sub_idx in idx_path {
            if self.children.len() == 0 {
                panic!("Index path too long")
            }
            output = &mut children[sub_idx];
        }
        output
    }
}
// endregion

// region index into io-data
impl<'a> IODataRead<'a> {
    fn index(&self, idx_path: &[usize]) -> IODataRead<'a> {
        let layout = &self.offsets[idx_path];
        let raw = layout.slice(self.raw);
        IODataRead {
            rust_type: &self.rust_type[idx_path],
            type_nullability: &self.type_nullability[idx_path],
            offsets: &self.offsets[idx_path],
            raw_nullability: &self.raw_nullability[idx_path],
            raw
        }
    }
}

impl<'a> IODataWrite<'a> {
    fn index_mut(&mut self, idx_path: &[usize]) -> IODataWrite<'a> {
        let layout = &self.offsets[idx_path];
        let raw = layout.slice_mut(self.raw);
        IODataWrite {
            rust_type: &self.rust_type[idx_path],
            type_nullability: &self.type_nullability[idx_path],
            offsets: &self.offsets[idx_path],
            raw_nullability: &mut self.raw_nullability[idx_path],
            raw
        }
    }
}
// endregion

// region access and modify raw data
impl<'a> IODataRead<'a> {
    /// Ref to raw data interpreted as `MaybeUninit<Value>`, unchecked
    pub unsafe fn coerce_raw<T>(&self) -> &MaybeUninit<T> {
        &*(self.raw.as_ptr() as *const MaybeUninit<T>)
    }

    /// Actual nullability of the data.
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    pub fn value_nullability(&self) -> &Nullability {
        &self.raw_nullability
    }
}

impl<'a> IODataWrite<'a> {
    /// Mutable ref to raw data interpreted as `MaybeUninit<Value>`, unchecked
    pub unsafe fn coerce_raw_mut<T>(&mut self) -> &mut MaybeUninit<T> {
        &mut *(self.raw.as_mut_ptr() as *mut MaybeUninit<T>)
    }

    /// Actual nullability of the data.
    /// Null = uninitialized, Partial = some fields or elems may be uninitialized
    pub fn value_nullability_mut(&mut self) -> &mut Nullability {
        &mut self.raw_nullability
    }
}
// endregion

// region read and write data
impl<'a> IODataRead<'a> {
    /// Read entire data.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn read_unchecked<Value: IODataType>(&self) -> Value {
        Value::_read_unchecked(self)
    }

    /// Read entire data.
    ///
    /// Returns `Err` if `Value` is the wrong type
    pub fn read<Value: IODataType>(&self) -> IOTypeCheckResult<Value> {
        self.as_check().check_type::<Value>()?;
        self.as_check().check_value_nullability()?;
        Ok(unsafe { self.read_unchecked::<Value>() })
    }
}

impl<'a> IODataWrite<'a> {
    /// Write entire data.
    ///
    /// SAFETY: UB if `Value` is the wrong type
    pub unsafe fn write_unchecked<Value: IODataType>(&mut self, value: Value) {
        value._write_unchecked(self)
    }

    /// Write entire data.
    ///
    /// Returns `Err` if `Value` is the wrong type
    pub fn write<Value: IODataType>(&mut self, value: Value) -> IOTypeCheckResult<()> {
        self.as_check().check_type::<Value>()?;
        self.as_check().check_value_nullability()?;
        Ok(unsafe { self.write_unchecked::<Value>(value) })
    }
}
// endregion

// region read and write at index (for compute graph)
impl<'a> IODataRead<'a> {
    // region access raw data
    /// Access raw data slice and nullability at the give index
    pub fn raw_data_idx(&self, idx: usize) -> (&RawData, &Nullability) {
        match self.raw_nullability {
            Nullability::Partial(ref child_nullabilities) => {
                let nullability = &child_nullabilities[idx];
                let raw = self.offsets.children[idx].slice(self.raw);
                (raw, nullability)
            }
            _ => panic!("Cannot access raw data at index of non-partial data")
        }
    }
}

impl<'a> IODataWrite<'a> {
    /// Mutable access to raw data slice and nullability at the give index.
    ///
    /// SAFETY: Modifications must preserve the shape
    pub unsafe fn raw_data_idx_mut(&mut self, idx: usize) -> (&mut RawData, &mut Nullability) {
        match self.raw_nullability {
            Nullability::Partial(ref child_nullabilities) => {
                let nullability = &mut child_nullabilities[idx];
                let raw = self.offsets.children[idx].slice_mut(self.raw);
                (raw, nullability)
            }
            _ => panic!("Cannot access raw data at index of non-partial data")
        }
    }

    /// Mutable access to raw data slice and nullability at each index.
    ///
    /// SAFETY: Modifications must preserve the shape
    pub unsafe fn iter_raw_data_mut(&mut self) -> impl Iterator<Item=(&mut RawData, &mut Nullability)> {
        match self.raw_nullability {
            Nullability::Partial(ref child_nullabilities) => {
                let raw_ptr = self.raw.as_mut_ptr();
                zip(&self.offsets.children, child_nullabilities)
                    .map(|(offset, nullability)| (offset.slice_mut_ptr(self.raw), nullability))
            }
            _ => panic!("Cannot access raw data at index of non-partial data")
        }
    }
}
// endregion

// region copy
impl IOData {
    /// Copy raw data into the other [IOData].
    ///
    /// SAFETY: UB if the types or type nullabilities are different.
    pub unsafe fn copy_data_into_unchecked(&self, dest: &mut Self) {
        debug_assert_eq!(self.raw.len(), dest.raw.len());
        unsafe { copy_nonoverlapping(self.raw.as_ptr(), dest.raw.as_mut_ptr(), self.raw.len()); }
        self.raw_nullability.clone_into(&mut dest.raw_nullability);
    }
}
// endregion

// region errors
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
// endregion

#[cfg(test)]
mod tests {
    use structural_reflection::c_tuple::{c_tuple, CTuple};
    use crate::raw::{IOData, LoadData, NonNull, Nullable};

    #[test]
    pub fn regression_test_io_data() {
        let input_data = c_tuple!(
            Nullable::Some("Placeholder"),
            NonNull("Text"),
            Nullable::<bool>::None,
            Nullable::<()>::None
        );
        let inputs = IOData::init(input_data);
        assert_eq!(inputs.load_all::<CTuple!(
            Nullable<&str>,
            NonNull<&str>,
            Nullable<bool>,
            Nullable<()>
        )>(), Ok(input_data));
    }
}