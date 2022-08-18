use std::collections::{BTreeMap, HashMap, LinkedList, VecDeque};
use derive_more::{Display, Error};

#[derive(Debug, Display, Error, Clone, PartialEq, Eq)]
#[display(fmt = "not found: {}", index)]
pub struct NotFound<Idx> {
    pub index: Idx
}

pub trait TryIndex<Idx> {
    type Output;

    fn try_index(&self, index: Idx) -> Result<&Self::Output, NotFound<Idx>>;

    fn get(&self, index: Idx) -> Result<&Self::Output, NotFound<Idx>> {
        self.try_index(index)
    }
}

pub trait TryIndexMut<Idx>: TryIndex<Idx> {
    fn try_index_mut(&mut self, index: Idx) -> Result<&mut Self::Output, NotFound<Idx>>;

    fn get_mut(&mut self, index: Idx) -> Result<&mut Self::Output, NotFound<Idx>> {
        self.try_index_mut(index)
    }
}

impl<'a, T> TryIndex<usize> for &'a [T] {
    type Output = T;

    fn try_index(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<'a, T> TryIndex<usize> for &'a mut [T] {
    type Output = T;

    fn try_index(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<'a, T> TryIndexMut<usize> for &'a mut [T] {
    fn try_index_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for Vec<T> {
    type Output = T;

    fn try_index(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for Vec<T> {
    fn try_index_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for VecDeque<T> {
    type Output = T;

    fn try_index(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for VecDeque<T> {
    fn try_index_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for LinkedList<T> {
    type Output = T;

    fn try_index(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for LinkedList<T> {
    fn try_index_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndex<K> for BTreeMap<K, V> {
    type Output = V;

    fn try_index(&self, index: K) -> Result<&Self::Output, NotFound<K>> {
        self.get(&index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndexMut<K> for BTreeMap<K, V> {
    fn try_index_mut(&mut self, index: K) -> Result<&mut Self::Output, NotFound<K>> {
        self.get_mut(&index).ok_or(NotFound { index })
    }
}

impl<'a, K, V> TryIndex<&'a K> for HashMap<K, V> {
    type Output = V;

    fn try_index(&self, index: &'a K) -> Result<&Self::Output, NotFound<&'a K>> {
        self.get(&index).ok_or(NotFound { index })
    }
}

impl<'a, K, V> TryIndexMut<&'a K> for HashMap<K, V> {
    fn try_index_mut(&mut self, index: &'a K) -> Result<&mut Self::Output, NotFound<&'a K>> {
        self.get_mut(&index).ok_or(NotFound { index })
    }
}