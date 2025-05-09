use std::{hash, mem, ops::Deref, ptr};

use derive_where::derive_where;

use super::Gcx;

#[derive_where(Copy, Clone)]
pub struct Def<'gcx, T>(&'gcx T);

impl<'a, T> Def<'a, T> {
    pub fn new_alloc(gcx: Gcx<'a>, val: T) -> Self {
        Self::new(gcx.alloc(val))
    }

    pub const fn new(value: &'a T) -> Self {
        const {
            if mem::size_of::<T>() == 0 {
                panic!("cannot use ZSTs as `Def`s")
            }
        }

        Self(value)
    }
}

impl<'a, T> Deref for Def<'a, T> {
    type Target = &'a T;

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
