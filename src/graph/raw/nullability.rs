use std::fmt::{Display, Formatter};
use std::iter::{repeat, zip};
use std::ops::Index;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NullRegion {
    /// Entire region is null
    Null,
    /// Entire region is not null
    NonNull,
    /// Region is for a struct or array where parts may be null
    Partial(Vec<NullRegion>)
}

impl NullRegion {
    /// Intersects nullability. **panics** if the null regions are partial and have different lengths
    pub fn intersect(&mut self, rhs: &Self) {
        match (self, rhs) {
            (NullRegion::Null, NullRegion::Null) => {},
            (this @ NullRegion::Null, NullRegion::NonNull) => *this = NullRegion::NonNull,
            (this @ NullRegion::Null, NullRegion::Partial(other_elems)) => *this = NullRegion::Partial(other_elems.clone()),
            (NullRegion::NonNull, NullRegion::Null) => {},
            (NullRegion::NonNull, NullRegion::Partial(_)) => {},
            (NullRegion::NonNull, NullRegion::NonNull) => {},
            (NullRegion::Partial(_), NullRegion::Null) => {},
            (this @ NullRegion::Partial(_), NullRegion::NonNull) => *this = NullRegion::NonNull,
            (NullRegion::Partial(elems), NullRegion::Partial(other_elems)) => {
                assert_eq!(elems.len(), other_elems.len(), "tried to compare null regions of different shapes");
                for (elem, other_elem) in zip(elems, other_elems) {
                    elem.intersect(other_elem);
                }
            }
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
    pub fn subdivide(&self) -> impl Iterator<Item=&Self> {
        match self {
            NullRegion::Null => SubdivideIter::Repeat(repeat(&NullRegion::Null)),
            NullRegion::NonNull => SubdivideIter::Repeat(repeat(&NullRegion::NonNull)),
            NullRegion::Partial(elems) => SubdivideIter::Slice(elems.iter())
        }
    }

    /// - `Null` = iterator of infinite `Null`
    /// - `NonNull` = iterator of infinite `NonNull`
    /// - `Partial` = iterator of elems
    pub fn into_subdivide(self) -> impl Iterator<Item=Self> {
        match self {
            NullRegion::Null => SubdivideIntoIter::Repeat(repeat(NullRegion::Null)),
            NullRegion::NonNull => SubdivideIntoIter::Repeat(repeat(NullRegion::NonNull)),
            NullRegion::Partial(elems) => SubdivideIntoIter::Slice(elems.into_iter())
        }
    }
}

enum SubdivideIter<'a> {
    Repeat(std::iter::Repeat<&'a NullRegion>),
    Slice(std::slice::Iter<'a, NullRegion>),
}

enum SubdivideIntoIter {
    Repeat(std::iter::Repeat<NullRegion>),
    Slice(std::vec::IntoIter<NullRegion>),
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

impl Iterator for SubdivideIntoIter {
    type Item = NullRegion;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SubdivideIntoIter::Slice(iter) => iter.next(),
            SubdivideIntoIter::Repeat(iter) => iter.next()
        }
    }
}

/// Index into this region = get whether the part at index is null:
/// if this is `Partial`, indexes into the region. Otherwise returns `this`.
impl Index<usize> for NullRegion {
    type Output = Self;

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
