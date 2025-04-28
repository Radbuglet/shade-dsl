use std::{hash, mem, ops::Deref, ptr};

use derive_where::derive_where;

#[derive_where(Copy, Clone)]
pub struct Def<'gcx, T>(&'gcx T);

impl<'gcx, T> Def<'gcx, T> {
    pub const fn new(value: &'gcx T) -> Self {
        const {
            if mem::size_of::<T>() == 0 {
                panic!("cannot use ZSTs as `Def`s")
            }
        }

        Self(value)
    }
}

impl<'gcx, T> Deref for Def<'gcx, T> {
    type Target = &'gcx T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> hash::Hash for Def<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const T as *const ()).hash(state);
    }
}

impl<T> Eq for Def<'_, T> {}

impl<T> PartialEq for Def<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}
