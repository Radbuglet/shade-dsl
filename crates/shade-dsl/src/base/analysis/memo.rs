use std::{cell::RefCell, hash, rc::Rc};

use ctx2d_utils::hash::{FxHashMap, hash_map};
use derive_where::derive_where;

use crate::base::{Diag, ErrorGuaranteed};

#[derive_where(Clone, Default)]
pub struct Memo<K, V> {
    entries: Rc<RefCell<FxHashMap<K, Option<Result<V, ErrorGuaranteed>>>>>,
}

impl<K, V> Memo<K, V>
where
    K: Clone + hash::Hash + Eq,
    V: Clone,
{
    pub fn compute(
        &self,
        key: K,
        f: impl FnOnce(&K) -> Result<V, ErrorGuaranteed>,
    ) -> Result<V, ErrorGuaranteed> {
        match self.entries.borrow_mut().entry(key.clone()) {
            hash_map::Entry::Occupied(entry) => {
                return match entry.get() {
                    Some(v) => v.clone(),
                    None => Err(Diag::anon_err("cycle detected :(").emit()),
                };
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
