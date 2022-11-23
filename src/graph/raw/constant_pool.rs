use std::collections::HashSet;
use std::marker::PhantomPinned;
use std::pin::Pin;

/// Contains all of the constant *references* in the IR-graph.
/// Note that constants which are not references (e.g. numbers) are stored inline.
#[derive(Debug, Clone)]
pub struct ConstantPool(Pin<Box<ConstantPoolData>>);

#[derive(Debug, Clone)]
struct ConstantPoolData {
    strings: HashSet<String>,
    /// Cannot be pinned because outside references will point to constants here
    _p: PhantomPinned
}

/// Type of a constant stored in the constant pool
pub trait ConstantPoolType {
    type AsRef: ?Sized;

    #[deprecated(note = "Use `get_or_insert` instead")]
    #[doc(hidden)]
    fn _insert_into(self, pool: &mut ConstantPool) -> &Self::AsRef;
}

impl ConstantPool {
    pub fn new() -> Self {
        ConstantPool(Box::pin(ConstantPoolData::new()))
    }

    /// Retrieves the interned value or inserts it if not already present
    pub fn get_or_insert<T: ConstantPoolType>(&mut self, value: T) -> &T::AsRef {
        #[allow(deprecated)]
        value._insert_into(self)
    }

    /// Allows [ConstantPoolType]s to mutate the inner value, as none of their operations are unsafe
    unsafe fn inner(&mut self) -> &mut ConstantPoolData {
        self.0.as_mut().get_unchecked_mut()
    }
}

impl ConstantPoolData {
    pub fn new() -> Self {
        ConstantPoolData {
            strings: HashSet::new(),
            _p: PhantomPinned
        }
    }
}

impl ConstantPoolType for String {
    type AsRef = str;

    fn _insert_into(self, pool: &mut ConstantPool) -> &Self::AsRef {
        unsafe { pool.inner().strings.get_or_insert(self).as_str() }
    }
}