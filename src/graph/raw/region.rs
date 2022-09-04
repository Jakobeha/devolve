use std::ops::Index;
use crate::graph::mutable::NodeInput;

#[derive(Clone)]
pub enum NullRegion {
    /// Entire region is null
    Null,
    /// Entire region is not null
    NonNull,
    /// Region is for a struct or array where parts may be null
    Partial(Vec<NullRegion>)
}

impl NullRegion {
    /// Returns `Null` if a `Hole`, and `Partial` if an array or tuple
    pub fn of(input: &NodeInput) -> Self {
        match input {
            NodeInput::Hole => NullRegion::Null,
            NodeInput::Dep(_) => NullRegion::NonNull,
            NodeInput::Const(_) => NullRegion::NonNull,
            NodeInput::Array(elems) => NullRegion::Partial(elems.iter().map(|elem| NullRegion::of(elem)).collect()),
            NodeInput::Tuple(elems) => NullRegion::Partial(elems.iter().map(|elem| NullRegion::of(&elem.input)).collect())
        }
    }
}

/// Index into this region = get whether the part at index is null:
/// if this is `Partial`, indexes into the region. Otherwise returns `this`.
impl Index<usize> for NullRegion {
    type Output = NullRegion;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            NullRegion::Null => &self,
            NullRegion::NonNull => &self,
            NullRegion::Partial(regions) => &regions[index]
        }
    }
}
