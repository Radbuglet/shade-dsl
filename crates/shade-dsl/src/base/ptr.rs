use std::{cell::RefCell, fmt, hash, ops::Deref, ptr};

use ctx2d_utils::hash::FxHashSet;
use derive_where::derive_where;

#[derive_where(Copy, Clone)]
pub struct Id<'a, T: ?Sized>(&'a T);

impl<'a, T: ?Sized> Id<'a, T> {
    pub const fn new(val: &'a T) -> Self {
        Self(val)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Id<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static CYCLE_DETECTION: RefCell<FxHashSet<*const ()>> =
                RefCell::new(FxHashSet::default());
        }

        let as_ptr = self.0 as *const T as *const ();

        if CYCLE_DETECTION.with_borrow_mut(|cycles| cycles.insert(as_ptr)) {
            write!(f, "<cycle @ {:p}>", self.0 as *const T)?;
        } else {
            f.debug_tuple("P")
                .field(&(self.0 as *const T))
                .field(&self.0)
                .finish()?;

            CYCLE_DETECTION.with_borrow_mut(|cycles| cycles.remove(&as_ptr));
        }

        Ok(())
    }
}

impl<T: ?Sized> hash::Hash for Id<'_, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state);
    }
}

impl<T: ?Sized> Eq for Id<'_, T> {}

impl<T: ?Sized> PartialEq for Id<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}

impl<'a, T: ?Sized> Deref for Id<'a, T> {
    type Target = &'a T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
