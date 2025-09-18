use std::{
    any::{Any, TypeId},
    cell::{RefCell, UnsafeCell},
    fmt,
    hash::BuildHasherDefault,
    mem::MaybeUninit,
    num::NonZeroU64,
    ops::Deref,
    ptr::NonNull,
    sync::{
        Mutex,
        atomic::{AtomicU8, AtomicU64, Ordering::*},
    },
};

use bumpalo::Bump;
use ctx2d_utils::hash::FxHashSet;
use derive_where::derive_where;

use crate::base::Session;

// === IrArena === //

pub struct IrArena {
    generation: NonZeroU64,
    inner: Mutex<IrArenaInner>,
}

struct IrArenaInner {
    bump: bumpalo::Bump,
    singles: Vec<NonNull<dyn Any + Send + Sync>>,
}

impl fmt::Debug for IrArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrArena").finish_non_exhaustive()
    }
}

impl Default for IrArena {
    fn default() -> Self {
        static ID_GEN: AtomicU64 = AtomicU64::new(1);

        Self {
            generation: NonZeroU64::new(ID_GEN.fetch_add(1, Relaxed)).unwrap(),
            inner: Mutex::new(IrArenaInner {
                bump: Bump::new(),
                singles: Vec::new(),
            }),
        }
    }
}

impl Drop for IrArena {
    fn drop(&mut self) {
        let inner = self.inner.get_mut().unwrap();

        for &single in &inner.singles {
            unsafe { single.drop_in_place() };
        }
    }
}

#[derive_where(Copy, Clone, Hash, Eq, PartialEq)]
pub struct IrRef<T: 'static + Send + Sync> {
    generation: NonZeroU64,
    ptr: NonNull<T>,
}

unsafe impl<T: 'static + Send + Sync> Send for IrRef<T> {}

unsafe impl<T: 'static + Send + Sync> Sync for IrRef<T> {}

impl<T> fmt::Debug for IrRef<T>
where
    T: 'static + Send + Sync + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static REENTRANT_FMT: RefCell<FxHashSet<(TypeId, NonNull<()>)>> =
                const { RefCell::new(FxHashSet::with_hasher(BuildHasherDefault::new())) };
        }

        let key = (TypeId::of::<T>(), self.ptr.cast());

        write!(f, "{:?}", self.ptr)?;

        if REENTRANT_FMT.with_borrow_mut(|v| v.insert(key)) {
            let _guard = scopeguard::guard((), |()| {
                REENTRANT_FMT.with_borrow_mut(|v| v.remove(&key));
            });

            f.write_str(": ")?;
            self.r(&Session::fetch()).fmt(f)
        } else {
            Ok(())
        }
    }
}

impl<T: 'static + Send + Sync> IrRef<T> {
    pub fn new(value: T, s: &Session) -> Self {
        let mut inner = s.ir_arena.inner.lock().unwrap();

        let ptr = NonNull::from(inner.bump.alloc(value));
        inner.singles.push(ptr);

        Self {
            generation: s.ir_arena.generation,
            ptr,
        }
    }

    pub fn r(self, s: &Session) -> &T {
        assert_eq!(s.ir_arena.generation, self.generation);
        unsafe { self.ptr.as_ref() }
    }
}

// === LateInit === //

pub struct LateInit<T> {
    is_init: AtomicU8,
    cell: UnsafeCell<MaybeUninit<T>>,
}

unsafe impl<T: Send> Send for LateInit<T> {}

unsafe impl<T: Send + Sync> Sync for LateInit<T> {}

impl<T: fmt::Debug> fmt::Debug for LateInit<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(value) = Self::get(self) {
            value.fmt(f)
        } else {
            f.write_str("<uninit>")
        }
    }
}

impl<T: Clone> Clone for LateInit<T> {
    fn clone(&self) -> Self {
        if let Some(value) = Self::get(self) {
            Self::new(value.clone())
        } else {
            Self::uninit()
        }
    }
}

impl<T> From<T> for LateInit<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T> LateInit<T> {
    pub const fn new(value: T) -> Self {
        Self {
            is_init: AtomicU8::new(2),
            cell: UnsafeCell::new(MaybeUninit::new(value)),
        }
    }

    pub const fn uninit() -> Self {
        Self {
            is_init: AtomicU8::new(0),
            cell: UnsafeCell::new(MaybeUninit::uninit()),
        }
    }

    pub fn init(me: &Self, value: T) {
        // Ensure that the cell is only initialized once.
        if me.is_init.compare_exchange(0, 1, Relaxed, Relaxed).is_err() {
            panic!("cannot initialize `LateInit` more than once");
        }

        // Initialize the cell.
        unsafe { *me.cell.get() = MaybeUninit::new(value) };

        // Mark the cell as initialized.
        me.is_init.store(2, Release);
    }

    pub fn get(me: &Self) -> Option<&T> {
        (me.is_init.load(Acquire) == 2).then(|| unsafe { (*me.cell.get()).assume_init_ref() })
    }
}

impl<T> Deref for LateInit<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        Self::get(self).expect("cell not initialized")
    }
}

impl<T> Drop for LateInit<T> {
    fn drop(&mut self) {
        if self.is_init.load(Acquire) != 2 {
            return;
        }

        unsafe { self.cell.get_mut().assume_init_drop() }
    }
}
