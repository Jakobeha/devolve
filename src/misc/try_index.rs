use std::collections::{BTreeMap, HashMap, LinkedList, VecDeque};
use derive_more::{Display, Error};

#[derive(Debug, Display, Error, Clone, PartialEq, Eq)]
#[display(fmt = "not found: {}", index)]
pub struct NotFound<Idx> {
    pub index: Idx
}

pub trait TryIndex<Idx> {
    type Output;

    fn get(&self, index: Idx) -> Result<&Self::Output, NotFound<Idx>>;
}

pub trait TryIndexMut<Idx>: TryIndex<Idx> {
    fn get_mut(&mut self, index: Idx) -> Result<&mut Self::Output, NotFound<Idx>>;
}

impl<'a, T> TryIndex<usize> for &'a [T] {
    type Output = T;

    fn get(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<'a, T> TryIndex<usize> for &'a mut [T] {
    type Output = T;

    fn get(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<'a, T> TryIndexMut<usize> for &'a mut [T] {
    fn get_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for Vec<T> {
    type Output = T;

    fn get(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for Vec<T> {
    fn get_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for VecDeque<T> {
    type Output = T;

    fn get(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for VecDeque<T> {
    fn get_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndex<usize> for LinkedList<T> {
    type Output = T;

    fn get(&self, index: usize) -> Result<&Self::Output, NotFound<usize>> {
        self.get(index).ok_or(NotFound { index })
    }
}

impl<T> TryIndexMut<usize> for LinkedList<T> {
    fn get_mut(&mut self, index: usize) -> Result<&mut Self::Output, NotFound<usize>> {
        self.get_mut(index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndex<K> for BTreeMap<K, V> {
    type Output = V;

    fn get(&self, index: K) -> Result<&Self::Output, NotFound<K>> {
        self.get(&index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndexMut<K> for BTreeMap<K, V> {
    fn get_mut(&mut self, index: K) -> Result<&mut Self::Output, NotFound<K>> {
        self.get_mut(&index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndex<K> for HashMap<K, V> {
    type Output = V;

    fn get(&self, index: K) -> Result<&Self::Output, NotFound<K>> {
        self.get(&index).ok_or(NotFound { index })
    }
}

impl<K, V> TryIndexMut<K> for HashMap<K, V> {
    fn get_mut(&mut self, index: K) -> Result<&mut Self::Output, NotFound<K>> {
        self.get_mut(&index).ok_or(NotFound { index })
    }
}