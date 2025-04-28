use std::{cell::RefCell, hash, ops::Deref, ptr};

use ctx2d_utils::hash::{FxHashMap, fx_hash_one, hash_map};
use derive_where::derive_where;

use super::Gcx;

// === Values === //

#[derive_where(Default)]
pub struct Interner<'gcx, T>(RefCell<FxHashMap<(u64, &'gcx T), ()>>);

impl<'gcx, T> Interner<'gcx, T>
where
    T: hash::Hash + Eq,
{
    pub fn intern(&self, gcx: Gcx<'gcx>, value: T) -> Intern<'gcx, T> {
        let mut inner = self.0.borrow_mut();

        let hash = fx_hash_one(&value);

        match inner
            .raw_entry_mut()
            .from_hash(hash, |(other_hash, other_value)| {
                hash == *other_hash && value == **other_value
            }) {
            hash_map::RawEntryMut::Occupied(entry) => Intern(entry.key().1),
            hash_map::RawEntryMut::Vacant(entry) => {
                let value = gcx.alloc(value);
                entry.insert_with_hasher(hash, (hash, value), (), |(hash, _)| *hash);
                Intern(value)
            }
        }
    }
}

#[derive_where(Copy, Clone)]
pub struct Intern<'gcx, T>(&'gcx T);

impl<'gcx, T> Deref for Intern<'gcx, T> {
    type Target = &'gcx T;

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

#[derive_where(Default)]
pub struct ListInterner<'gcx, T>(RefCell<FxHashMap<(u64, &'gcx [T]), ()>>);

impl<'gcx, T> ListInterner<'gcx, T>
where
    T: hash::Hash + Eq + Clone,
{
    pub fn intern(&self, gcx: Gcx<'gcx>, value: &[T]) -> InternList<'gcx, T> {
        if value.is_empty() {
            // TODO: ensure that the address of this is consistent
            return InternList(&[]);
        }

        let mut inner = self.0.borrow_mut();

        let hash = fx_hash_one(value);

        match inner
            .raw_entry_mut()
            .from_hash(hash, |(other_hash, other_value)| {
                hash == *other_hash && value == *other_value
            }) {
            hash_map::RawEntryMut::Occupied(entry) => InternList(entry.key().1),
            hash_map::RawEntryMut::Vacant(entry) => {
                let value = gcx.alloc_slice(value);
                entry.insert_with_hasher(hash, (hash, value), (), |(hash, _)| *hash);
                InternList(value)
            }
        }
    }

    pub fn intern_iter(
        &self,
        gcx: Gcx<'gcx>,
        value: impl IntoIterator<Item = T>,
    ) -> InternList<'gcx, T> {
        self.intern(gcx, &value.into_iter().collect::<Vec<_>>())
    }
}

#[derive_where(Copy, Clone)]
pub struct InternList<'gcx, T>(&'gcx [T]);

impl<'gcx, T> Deref for InternList<'gcx, T> {
    type Target = &'gcx [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> hash::Hash for InternList<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const [T] as *const ()).hash(state);
    }
}

impl<T> Eq for InternList<'_, T> {}

impl<T> PartialEq for InternList<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}
