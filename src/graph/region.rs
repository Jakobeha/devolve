use std::ops::Index;
use crate::graph::mutable::NodeInput;

#[derive(Clone)]
pub enum UsedRegion {
    Ignored,
    Used,
    /// Extract where the sub-regions are from type info
    Partial(Vec<UsedRegion>)
}

impl UsedRegion {
    /// Returns `Ignored` if a `Hole`, and `Partial` if an array or tuple
    pub fn of(input: &NodeInput) -> Self {
        match input {
            NodeInput::Hole => UsedRegion::Ignored,
            NodeInput::Dep(_) => UsedRegion::Used,
            NodeInput::Const(_) => UsedRegion::Used,
            NodeInput::Array(elems) => UsedRegion::Partial(elems.iter().map(|elem| UsedRegion::of(elem)).collect()),
            NodeInput::Tuple(elems) => UsedRegion::Partial(elems.iter().map(|elem| UsedRegion::of(&elem.input)).collect())
        }
    }
}

/// Index into this region = ignored or used if this is fully ignored or used, else index into partial.
impl Index<usize> for UsedRegion {
    type Output = UsedRegion;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            UsedRegion::Ignored => &self,
            UsedRegion::Used => &self,
            UsedRegion::Partial(regions) => &regions[index]
        }
    }
}
