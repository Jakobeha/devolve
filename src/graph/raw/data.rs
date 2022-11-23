use std::iter::zip;
use std::mem::{MaybeUninit, size_of};
use std::ptr::{copy_nonoverlapping, slice_from_raw_parts_mut};
use structural_reflection::{HasTypeName, infer_c_tuple_elem_offsets, infer_c_tuple_size, RustType};
use crate::raw::{NullRegion, IODataTypes};

/// Untyped input data for a node graph.
///
/// Implementation: type/memory-safe wrapper over [RawData] to be passed as input.
#[repr(transparent)]
pub struct InputData(IOData);

/// Output data holder for a node-graph.
///
/// Implementation: type/memory-safe wrapper over [RawData] to be passed as output.
#[repr(transparent)]
pub struct OutputData(IOData);

/// Raw input or output data for a node-graph which is typically manipulated via unsafe operations.
pub struct IOData {
    rust_types: Vec<RustType>,
    /// Non-null in input, required to be set in output
    null_regions: Vec<NullRegion>,
    offsets: Vec<usize>,
    raw: Box<RawData>
}

pub type RawData = [MaybeUninit<u8>];

impl InputData {
    pub fn new<Values: IODataTypes>(values: Values) -> InputData where <Values::Inner as HasTypeName>::StaticId: Sized {
        let (raw_data, null_regions) = values.split();
        InputData(IOData::new(
            Values::iter_rust_types().collect(),
            null_regions,
            raw_data
        ))
    }

    pub unsafe fn as_raw(&self) -> &IOData {
        &self.0
    }

    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    pub fn null_regions(&self) -> &[NullRegion] {
        self.0.null_regions()
    }

    pub fn data(&self, idx: usize) -> &RawData {
        self.0.data(idx)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl OutputData {
    pub fn new<Values: IODataTypes>() -> OutputData where <Values::Inner as HasTypeName>::StaticId: Sized {
        OutputData(IOData::new(
            Values::iter_rust_types().collect(),
            Values::iter_max_null_regions().collect(),
            MaybeUninit::<Values::Inner>::uninit()
        ))
    }

    pub fn with_checked<Values: IODataTypes>(fun: impl FnOnce(&mut OutputData)) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        let mut output = OutputData::new::<Values>();
        fun(&mut output);
        unsafe { output.as_raw() }.load_checked()
    }

    pub unsafe fn with<Values: IODataTypes>(fun: impl FnOnce(&mut OutputData)) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        let mut output = OutputData::new::<Values>();
        fun(&mut output);
        output.as_raw().load()
    }

    pub unsafe fn as_raw(&mut self) -> &mut IOData {
        &mut self.0
    }

    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    pub fn null_regions(&self) -> &[NullRegion] {
        self.0.null_regions()
    }

    pub fn data(&mut self, idx: usize) -> &mut RawData {
        self.0.data_mut(idx)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl IOData {
    pub fn new<T>(
        rust_types: Vec<RustType>,
        null_regions: Vec<NullRegion>,
        raw_data: MaybeUninit<T>
    ) -> IOData {
        assert_eq!(size_of::<T>(), infer_c_tuple_size(&rust_types), "size of actual type must equal inferred size of rust_types");
        let mut this = Self::new_uninit(rust_types, null_regions);

        unsafe {
            copy_nonoverlapping(&raw_data as *const _ as *const u8, this.raw.as_mut_ptr() as *mut u8, size_of::<T>());
        }

        this
    }

    pub fn new_uninit(
        rust_types: Vec<RustType>,
        null_regions: Vec<NullRegion>,
    ) -> IOData {
        assert_eq!(rust_types.len(), null_regions.len(), "# of types must be the same as # of null regions");

        let offsets = infer_c_tuple_elem_offsets(&rust_types).collect::<Vec<_>>();
        let raw = Box::new_uninit_slice(infer_c_tuple_size(&rust_types));

        IOData {
            rust_types,
            null_regions,
            offsets,
            raw
        }
    }

    pub unsafe fn all_data<T>(&self) -> &MaybeUninit<T> {
        assert_eq!(size_of::<T>(), infer_c_tuple_size(&self.rust_types), "size of actual type must equal inferred size of rust_types");
        &*(self.raw.as_ptr() as *const MaybeUninit<T>)
    }

    pub unsafe fn all_data_mut<T>(&mut self) -> &mut MaybeUninit<T> {
        assert_eq!(size_of::<T>(), infer_c_tuple_size(&self.rust_types), "size of actual type must equal inferred size of rust_types");
        &mut *(self.raw.as_mut_ptr() as *mut MaybeUninit<T>)
    }

    pub fn check<Values: IODataTypes>(&self) where <Values::Inner as HasTypeName>::StaticId: Sized {
        assert_eq!(self.len(), Values::len(), "number of values must match");
        for (actual_rust_type, expected_rust_type) in zip(self.rust_types(), Values::iter_rust_types()) {
            assert_eq!(actual_rust_type, &expected_rust_type, "rust type must match");
        }
        for (actual_null_region, expected_null_region) in zip(self.null_regions(), Values::iter_max_null_regions()) {
            assert_eq!(actual_null_region, &expected_null_region, "null region must match");
        }
    }

    pub fn load_checked<Values: IODataTypes>(&self) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.check::<Values>();
        unsafe { self.load() }
    }

    pub fn store_checked<Values: IODataTypes>(&mut self, values: Values) where <Values::Inner as HasTypeName>::StaticId: Sized {
        self.check::<Values>();
        unsafe { self.store(values) }
    }

    pub unsafe fn load<Values: IODataTypes>(&self) -> Values where <Values::Inner as HasTypeName>::StaticId: Sized {
        // TODO: Set nullability in output?
        let inner = *self.all_data::<Values::Inner>();
        Values::new(inner, self.null_regions().to_vec())
    }

    pub unsafe fn store<Values: IODataTypes>(&mut self, values: Values) where <Values::Inner as HasTypeName>::StaticId: Sized {
        let (data, _) = values.split();
        *self.all_data_mut::<Values::Inner>() = data;
    }


    pub unsafe fn as_input(&self) -> &InputData {
        &*(self as *const Self as *const InputData)
    }

    pub unsafe fn as_output(&mut self) -> &mut OutputData {
        &mut *(self as *mut Self as *mut OutputData)
    }

    pub fn rust_types(&self) -> &[RustType] {
        &self.rust_types
    }

    pub fn null_regions(&self) -> &[NullRegion] {
        &self.null_regions
    }

    pub fn len(&self) -> usize {
        debug_assert_eq!(self.rust_types.len(), self.null_regions.len(), "sanity check failed");
        self.rust_types.len()
    }

    pub fn data(&self, idx: usize) -> &RawData {
        let (offset, size) = self.offset_size(idx);
        &self.raw[offset..offset + size]
    }

    pub fn data_mut(&mut self, idx: usize) -> &mut RawData {
        let (offset, size) = self.offset_size(idx);
        &mut self.raw[offset..offset + size]
    }

    unsafe fn data_mut_ptr(rust_types: &[RustType], offsets: &[usize], raw: *mut MaybeUninit<u8>, idx: usize) -> *mut RawData {
        let (offset, size) = Self::offset_size_ptr(rust_types, offsets, idx);
        slice_from_raw_parts_mut(raw.add(offset), size)
    }

    pub fn iter_data(&self) -> impl Iterator<Item=&RawData> {
        (0..self.len()).map(move |idx| self.data(idx))
    }

    pub fn iter_data_mut(&mut self) -> impl Iterator<Item=&mut RawData> {
        let rust_types = &self.rust_types;
        let offsets = &self.offsets;
        let raw = self.raw.as_mut_ptr();
        (0..self.len()).map(move |idx| unsafe { &mut *Self::data_mut_ptr(rust_types, offsets, raw, idx) })
    }

    pub fn copy_data_into(&self, dest: &mut Self) {
        debug_assert_eq!(self.raw.len(), dest.raw.len());
        unsafe { copy_nonoverlapping(self.raw.as_ptr(), dest.raw.as_mut_ptr(), self.raw.len()); }
    }

    fn offset_size(&self, idx: usize) -> (usize, usize) {
        (self.offsets[idx], self.rust_types[idx].size)
    }

    fn offset_size_ptr(rust_types: &[RustType], offsets: &[usize], idx: usize) -> (usize, usize) {
        (offsets[idx], rust_types[idx].size)
    }
}