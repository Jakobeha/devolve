use std::fmt::{Display, Formatter};
use std::iter::{repeat, zip};
use std::ops::Index;

/// Nullability AKA null region: describes whether a value *may be* (not necessarily is) null.
/// Values can be partially null if they are compound (tuples, arrays, structures, enum variants):
/// in this case, some fields may be null while some are non-null.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nullability {
    /// Entire region is null
    Null,
    /// Entire region is not null
    NonNull,
    /// Region is for a struct or array where parts may be null
    Partial(Vec<Nullability>)
}

impl Nullability {
    /// Intersects nullability. **panics** if the null regions are partial and have different lengths
    pub fn intersect(&mut self, rhs: &Self) {
        match (self, rhs) {
            (Nullability::Null, Nullability::Null) => {},
            (this @ Nullability::Null, Nullability::NonNull) => *this = Nullability::NonNull,
            (this @ Nullability::Null, Nullability::Partial(other_elems)) => *this = Nullability::Partial(other_elems.clone()),
            (Nullability::NonNull, Nullability::Null) => {},
            (Nullability::NonNull, Nullability::Partial(_)) => {},
            (Nullability::NonNull, Nullability::NonNull) => {},
            (Nullability::Partial(_), Nullability::Null) => {},
            (this @ Nullability::Partial(_), Nullability::NonNull) => *this = Nullability::NonNull,
            (Nullability::Partial(elems), Nullability::Partial(other_elems)) => {
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
            (Nullability::Null, Nullability::Null) => true,
            (Nullability::Null, Nullability::NonNull) => false,
            (Nullability::Null, Nullability::Partial(other_elems)) => other_elems.iter().all(|other_elem| Nullability::Null.is_subset_of(other_elem)),
            (Nullability::NonNull, Nullability::Null) => true,
            (Nullability::NonNull, Nullability::Partial(_)) => true,
            (Nullability::NonNull, Nullability::NonNull) => true,
            (Nullability::Partial(_), Nullability::Null) => true,
            (Nullability::Partial(elems), Nullability::NonNull) => elems.iter().all(|elem| elem.is_subset_of(&Nullability::NonNull)),
            (Nullability::Partial(elems), Nullability::Partial(other_elems)) => {
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
            Nullability::Null => SubdivideIter::Repeat(repeat(&Nullability::Null)),
            Nullability::NonNull => SubdivideIter::Repeat(repeat(&Nullability::NonNull)),
            Nullability::Partial(elems) => SubdivideIter::Slice(elems.iter())
        }
    }

    /// - `Null` = converts into `Partial` of `len` `Null`, iterates over those
    /// - `NonNull` = converts into `Partial` of `len` `NonNull`, iterates over those
    /// - `Partial` = iterator of elems
    pub fn subdivide_mut(&mut self, len: usize) -> &mut [Self] {
        match self {
            Nullability::Null => *self = Nullability::Partial(vec![Nullability::Null; len]),
            Nullability::NonNull => *self = Nullability::Partial(vec![Nullability::NonNull; len]),
            _ => {}
        }
        match self {
            Nullability::Partial(elems) => elems,
            _ => unreachable!()
        }
    }

    /// - `Null` = iterator of infinite `Null`
    /// - `NonNull` = iterator of infinite `NonNull`
    /// - `Partial` = iterator of elems
    pub fn into_subdivide(self) -> impl Iterator<Item=Self> {
        match self {
            Nullability::Null => SubdivideIntoIter::Repeat(repeat(Nullability::Null)),
            Nullability::NonNull => SubdivideIntoIter::Repeat(repeat(Nullability::NonNull)),
            Nullability::Partial(elems) => SubdivideIntoIter::Slice(elems.into_iter())
        }
    }
}

enum SubdivideIter<'a> {
    Repeat(std::iter::Repeat<&'a Nullability>),
    Slice(std::slice::Iter<'a, Nullability>),
}

enum SubdivideIntoIter {
    Repeat(std::iter::Repeat<Nullability>),
    Slice(std::vec::IntoIter<Nullability>),
}

impl<'a> Iterator for SubdivideIter<'a> {
    type Item = &'a Nullability;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SubdivideIter::Slice(iter) => iter.next(),
            SubdivideIter::Repeat(iter) => iter.next()
        }
    }
}

impl Iterator for SubdivideIntoIter {
    type Item = Nullability;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SubdivideIntoIter::Slice(iter) => iter.next(),
            SubdivideIntoIter::Repeat(iter) => iter.next()
        }
    }
}

/// Index into this region = get whether the part at index is null:
/// if this is `Partial`, indexes into the region. Otherwise returns `this`.
impl Index<usize> for Nullability {
    type Output = Self;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Nullability::Null => &self,
            Nullability::NonNull => &self,
            Nullability::Partial(regions) => &regions[index]
        }
    }
}

impl Display for Nullability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nullability::Null => write!(f, "?"),
            Nullability::NonNull => write!(f, "!"),
            Nullability::Partial(regions) => {
                write!(f, "[")?;
                for region in regions {
                    write!(f, "{}", region)?;
                }
                write!(f, "]")
            }
        }
    }
}
