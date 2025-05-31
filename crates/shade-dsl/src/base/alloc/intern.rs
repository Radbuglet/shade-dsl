use std::{cell::RefCell, hash, ops::Deref, ptr};

use ctx2d_utils::{
    hash::{FxHashMap, fx_hash_one, hash_map},
    mem::MayDangle,
    my_may_dangle,
};
use derive_where::derive_where;

// === Values === //

#[allow(clippy::type_complexity)]
pub struct Interner<'a, T>(RefCell<my_may_dangle!(FxHashMap<(u64, &'a T), ()>)>);

impl<T> Default for Interner<'_, T> {
    fn default() -> Self {
        Self(RefCell::new(unsafe { MayDangle::new_default() }))
    }
}

impl<'a, T> Interner<'a, T>
where
    T: hash::Hash + Eq,
{
    pub fn intern(&self, bump: &'a bumpalo::Bump, value: T) -> Intern<'a, T> {
        let mut inner = self.0.borrow_mut();

        let hash = fx_hash_one(&value);

        match inner
            .raw_entry_mut()
            .from_hash(hash, |(other_hash, other_value)| {
                hash == *other_hash && value == **other_value
            }) {
            hash_map::RawEntryMut::Occupied(entry) => Intern(entry.key().1),
            hash_map::RawEntryMut::Vacant(entry) => {
                let value = bump.alloc(value);
                entry.insert_with_hasher(hash, (hash, value), (), |(hash, _)| *hash);
                Intern(value)
            }
        }
    }
}

#[derive_where(Copy, Clone)]
pub struct Intern<'a, T>(&'a T);

impl<'a, T> Deref for Intern<'a, T> {
    type Target = &'a T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> hash::Hash for Intern<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const T as *const ()).hash(state);
    }
}

impl<T> Eq for Intern<'_, T> {}

impl<T> PartialEq for Intern<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}

// === Lists === //

#[allow(clippy::type_complexity)]
pub struct ListInterner<'a, T>(RefCell<my_may_dangle!(FxHashMap<(u64, &'a [T]), ()>)>);

impl<T> Default for ListInterner<'_, T> {
    fn default() -> Self {
        Self(RefCell::new(unsafe { MayDangle::new_default() }))
    }
}

impl<'a, T> ListInterner<'a, T>
where
    T: hash::Hash + Eq + Clone,
{
    pub fn intern(&self, bump: &'a bumpalo::Bump, value: &[T]) -> ListIntern<'a, T> {
        if value.is_empty() {
            // TODO: ensure that the address of this is consistent
            return ListIntern(&[]);
        }

        let mut inner = self.0.borrow_mut();

        let hash = fx_hash_one(value);

        match inner
            .raw_entry_mut()
            .from_hash(hash, |(other_hash, other_value)| {
                hash == *other_hash && value == *other_value
            }) {
            hash_map::RawEntryMut::Occupied(entry) => ListIntern(entry.key().1),
            hash_map::RawEntryMut::Vacant(entry) => {
                let value = bump.alloc_slice_clone(value);
                entry.insert_with_hasher(hash, (hash, value), (), |(hash, _)| *hash);
                ListIntern(value)
            }
        }
    }

    pub fn intern_iter(
        &self,
        bump: &'a bumpalo::Bump,
        value: impl IntoIterator<Item = T>,
    ) -> ListIntern<'a, T> {
        self.intern(bump, &value.into_iter().collect::<Vec<_>>())
    }
}

#[derive_where(Copy, Clone)]
pub struct ListIntern<'a, T>(&'a [T]);

impl<'a, T> Deref for ListIntern<'a, T> {
    type Target = &'a [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> hash::Hash for ListIntern<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const [T] as *const ()).hash(state);
    }
}

impl<T> Eq for ListIntern<'_, T> {}

impl<T> PartialEq for ListIntern<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}
