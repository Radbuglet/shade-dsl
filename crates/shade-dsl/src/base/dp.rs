use std::{cell::RefCell, hash, rc::Rc};

use ctx2d_utils::hash::{FxHashMap, hash_map};
use derive_where::derive_where;

#[derive_where(Clone, Default)]
pub struct Dp<K, V> {
    entries: Rc<RefCell<FxHashMap<K, Option<V>>>>,
}

impl<K, V> Dp<K, V>
where
    K: Clone + hash::Hash + Eq,
    V: Clone,
{
    pub fn compute(&self, key: K, f: impl FnOnce(&K) -> V) -> V {
        match self.entries.borrow_mut().entry(key.clone()) {
            hash_map::Entry::Occupied(entry) => {
                return entry.get().as_ref().expect("cycle detected").clone();
            }
            hash_map::Entry::Vacant(entry) => {
                entry.insert(None);
            }
        }

        let value = f(&key);
        self.entries.borrow_mut().insert(key, Some(value.clone()));
        value
    }
}
