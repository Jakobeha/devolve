use std::mem::{forget, size_of};
use std::ptr::{read, slice_from_raw_parts};

/// Inline untyped pointer data, may be a thin or fat pointer but stored as the largest possible pointer size.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InlinePtr {
    ptr: *const (),
    metadata: InlinePtrMetadata
}

unsafe impl Sync for InlinePtr {}
unsafe impl Send for InlinePtr {}

/// Currently all pointer metadata fits in a usize
type InlinePtrMetadata = usize;

impl InlinePtr {
    pub fn new<T: ?Sized>(ptr: *const T) -> InlinePtr {
        let size = size_of::<*const T>();
        if size == size_of::<*const ()>() {
            InlinePtr {
                ptr: ptr as *const (),
                metadata: 0
            }
        } else if size == size_of::<(*const (), usize)>() {
            let (ptr, metadata) = unsafe { unsafe_transmute::<*const T, (*const (), usize)>(ptr) };
            InlinePtr {
                ptr,
                metadata
            }
        } else {
            panic!("Unsupported pointer size: {}", size);
        }
    }

    /// Pointer to the pointer data, size is unknown
    pub fn ptr_to_pointer_data(&self) -> *const () {
        self as *const InlinePtr as *const ()
    }

    /// Represents this pointer as a u8 slice.
    /// This can be done for any kind of POD slice and string slices.
    pub fn as_u8_slice_ptr(&self) -> *const [u8] {
        slice_from_raw_parts(self.ptr as *const u8, self.metadata)
    }
}

/// Transmute between unknown sizes
pub unsafe fn unsafe_transmute<T, U>(t: T) -> U {
    let u = read(&t as *const T as *const U);
    forget(t);
    u
}