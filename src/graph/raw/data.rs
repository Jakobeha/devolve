use std::iter::zip;
use std::mem::MaybeUninit;
use std::ptr::copy_nonoverlapping;
use structural_reflection::{infer_c_tuple_elem_offsets, infer_c_tuple_size, RustType};
use crate::raw::{NullRegion, IODataType};

/// Input data holder for a node-graph.
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
    pub fn new<Values: IODataType>(values: Values::OfOptionals) -> InputData where Values::StaticId: Sized {
        InputData(IOData::new(
            Values::iter_rust_types().collect(),
            Values::iter_is_some(&values).map(|is_some| if is_some { NullRegion::NonNull } else { NullRegion::Null }).collect(),
            Values::iter_raw_data(&values).collect()
        ))
    }

    pub unsafe fn as_raw(&self) -> &IOData {
        &self.0
    }

    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    pub fn nonnull_regions(&self) -> &[NullRegion] {
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
    pub unsafe fn as_raw(&mut self) -> &mut IOData {
        &mut self.0
    }

    pub fn rust_types(&self) -> &[RustType] {
        self.0.rust_types()
    }

    pub fn nonnull_regions(&self) -> &[NullRegion] {
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
    pub fn new(
        rust_types: Vec<RustType>,
        null_regions: Vec<NullRegion>,
        raw_elems: impl IntoIterator<Item=*const ()>
    ) -> IOData {
        assert_eq!(rust_types.len(), null_regions.len(), "# of types must be the same as # of null regions");

        let mut offsets = infer_c_tuple_elem_offsets(&rust_types).collect::<Vec<_>>();
        let mut raw = Box::new_uninit_slice(infer_c_tuple_size(&rust_types));
        for ((rust_type, offset), raw_elem) in zip(zip(&rust_types, &offsets), raw_elems) {
            unsafe { copy_nonoverlapping(raw_elem, raw.as_mut_ptr().add(*offset), rust_type.size); }
        }

        IOData {
            rust_types,
            null_regions,
            offsets,
            raw
        }
    }

    pub unsafe fn as_input(&self) -> &InputData {
        &*(raw as *const Self as *const InputData)
    }

    pub unsafe fn as_output(&mut self) -> &mut OutputData {
        &mut *(raw as *mut Self as *mut OutputData)
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
        &*self.raw[offset..offset + size]
    }

    pub fn data_mut(&mut self, idx: usize) -> &mut RawData {
        let (offset, size) = self.offset_size(idx);
        &mut *self.raw[offset..offset + size]
    }

    fn offset_size(&self, idx: usize) -> (usize, usize) {
        (self.offsets[idx], self.rust_types[idx].size)
    }
}