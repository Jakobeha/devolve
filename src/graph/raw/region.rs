use std::fmt::{Display, Formatter};
use std::iter::{repeat, zip};
use std::ops::Index;
use crate::graph::ir::NodeInput;

#[derive(Debug, Clone)]
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
    #[deprecated]
    pub fn of(input: &NodeInput) -> Self {
        match input {
            NodeInput::Hole => NullRegion::Null,
            NodeInput::Dep(_) => NullRegion::NonNull,
            NodeInput::Const(_) => NullRegion::NonNull,
            NodeInput::Array(elems) => NullRegion::Partial(elems.iter().map(|elem| NullRegion::of(elem)).collect()),
            NodeInput::Tuple(elems) => NullRegion::Partial(elems.iter().map(|elem| NullRegion::of(&elem.input)).collect())
        }
    }

    /// Whether the region's nullability is a subset of the other region:
    ///
    /// - `Null` is bottom (superset of everything)
    /// - `NonNull` is top (subset of everything)
    /// - **panics** if the regions are partial with different lengths
    /// - otherwise, evaluates `a[i] <= b[i]` for each element
    pub fn is_subset_of(&self, other: &Self) -> bool {
        match (self, other) {
            (NullRegion::Null, NullRegion::Null) => true,
            (NullRegion::Null, NullRegion::NonNull) => false,
            (NullRegion::Null, NullRegion::Partial(other_elems)) => other_elems.iter().all(|other_elem| NullRegion::Null.is_subset_of(other_elem)),
            (NullRegion::NonNull, NullRegion::Null) => true,
            (NullRegion::NonNull, NullRegion::Partial(_)) => true,
            (NullRegion::NonNull, NullRegion::NonNull) => true,
            (NullRegion::Partial(_), NullRegion::Null) => true,
            (NullRegion::Partial(elems), NullRegion::NonNull) => elems.iter().all(|elem| elem.is_subset_of(&NullRegion::NonNull)),
            (NullRegion::Partial(elems), NullRegion::Partial(other_elems)) => {
                assert_eq!(elems.len(), other_elems.len(), "tried to compare null regions of different shapes");
                zip(elems, other_elems).all(|(elem, other_elem)| elem.is_subset_of(other_elem))
            }
        }
    }

    /// - `Null` = iterator of infinite `Null`
    /// - `NonNull` = iterator of infinite `NonNull`
    /// - `Partial` = iterator of elems
    pub fn subdivide(&self) -> impl Iterator<Item=&NullRegion> {
        match self {
            NullRegion::Null => SubdivideIter::Repeat(repeat(&NullRegion::Null)),
            NullRegion::NonNull => SubdivideIter::Repeat(repeat(&NullRegion::NonNull)),
            NullRegion::Partial(elems) => SubdivideIter::Slice(elems.iter())
        }
    }
}

enum SubdivideIter<'a> {
    Repeat(std::iter::Repeat<&'a NullRegion>),
    Slice(std::slice::Iter<'a, NullRegion>),
}

impl<'a> Iterator for SubdivideIter<'a> {
    type Item = &'a NullRegion;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SubdivideIter::Slice(iter) => iter.next(),
            SubdivideIter::Repeat(iter) => iter.next()
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

impl Display for NullRegion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NullRegion::Null => write!(f, "?"),
            NullRegion::NonNull => write!(f, "!"),
            NullRegion::Partial(regions) => {
                write!(f, "[")?;
                for region in regions {
                    write!(f, "{}", region)?;
                }
                write!(f, "]")
            }
        }
    }
}
