use std::{cell::RefCell, hash, ops::Deref, ptr};

use ctx2d_utils::hash::{FxHashMap, fx_hash_one, hash_map};
use derive_where::derive_where;

// === Values === //

#[derive_where(Default)]
pub struct Interner<'a, T>(RefCell<FxHashMap<(u64, &'a T), ()>>);

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

#[derive_where(Default)]
pub struct ListInterner<'a, T>(RefCell<FxHashMap<(u64, &'a [T]), ()>>);

impl<'a, T> ListInterner<'a, T>
where
    T: hash::Hash + Eq + Clone,
{
    pub fn intern(&self, bump: &'a bumpalo::Bump, value: &[T]) -> InternList<'a, T> {
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
                let value = bump.alloc_slice_clone(value);
                entry.insert_with_hasher(hash, (hash, value), (), |(hash, _)| *hash);
                InternList(value)
            }
        }
    }

    pub fn intern_iter(
        &self,
        bump: &'a bumpalo::Bump,
        value: impl IntoIterator<Item = T>,
    ) -> InternList<'a, T> {
        self.intern(bump, &value.into_iter().collect::<Vec<_>>())
    }
}

#[derive_where(Copy, Clone)]
pub struct InternList<'a, T>(&'a [T]);

impl<'a, T> Deref for InternList<'a, T> {
    type Target = &'a [T];

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
