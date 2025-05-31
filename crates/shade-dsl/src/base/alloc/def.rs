use std::{cell::OnceCell, hash, ops::Deref, ptr};

use derive_where::derive_where;

use crate::base::Gcx;

#[derive_where(Copy, Clone)]
pub struct Def<'gcx, T>(&'gcx DefInner<T>);

impl<'gcx, T> Def<'gcx, T> {
    pub fn new(gcx: Gcx<'gcx>, val: T) -> Self {
        let cell = OnceCell::new();
        cell.set(val).ok().unwrap();
        Self(gcx.alloc(DefInner(cell)))
    }

    pub fn new_uninit(gcx: Gcx<'gcx>) -> Self {
        Self(gcx.alloc(DefInner(OnceCell::new())))
    }

    pub fn init(me: Self, value: T) {
        me.0.0
            .set(value)
            .ok()
            .expect("cannot initialize a definition more than once")
    }
}

impl<'gcx, T> Deref for Def<'gcx, T> {
    type Target = &'gcx DefInner<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> hash::Hash for Def<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const DefInner<T> as *const ()).hash(state);
    }
}

impl<T> Eq for Def<'_, T> {}

impl<T> PartialEq for Def<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}

pub struct DefInner<T>(OnceCell<T>);

impl<T> Deref for DefInner<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.get().expect("definition never initialized")
    }
}
