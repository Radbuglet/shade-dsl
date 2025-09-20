use std::{
    any::{Any, TypeId},
    cell::{Cell, RefCell, UnsafeCell},
    fmt,
    hash::{self, BuildHasher as _, BuildHasherDefault},
    mem::MaybeUninit,
    num::NonZeroU64,
    ops::Deref,
    ptr::NonNull,
    sync::atomic::{AtomicU64, Ordering::*},
};

use bumpalo::Bump;
use derive_where::derive_where;

use crate::{
    base::Session,
    utils::hash::{FxBuildHasher, FxHashMap, FxHashSet, hash_map},
};

// === GpArena === //

pub struct GpArena {
    generation: NonZeroU64,
    inner: RefCell<GpArenaInner>,
}

struct GpArenaInner {
    bump: bumpalo::Bump,
    singles: Vec<NonNull<dyn Any + Send + Sync>>,
}

impl fmt::Debug for GpArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GpArena").finish_non_exhaustive()
    }
}

impl Default for GpArena {
    fn default() -> Self {
        static ID_GEN: AtomicU64 = AtomicU64::new(1);

        Self {
            generation: NonZeroU64::new(ID_GEN.fetch_add(1, Relaxed)).unwrap(),
            inner: RefCell::new(GpArenaInner {
                bump: Bump::new(),
                singles: Vec::new(),
            }),
        }
    }
}

impl Drop for GpArena {
    fn drop(&mut self) {
        let inner = self.inner.get_mut();

        for &single in &inner.singles {
            unsafe { single.drop_in_place() };
        }
    }
}

#[derive_where(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Obj<T: 'static + Send + Sync> {
    generation: NonZeroU64,
    ptr: NonNull<T>,
}

unsafe impl<T: 'static + Send + Sync> Send for Obj<T> {}

unsafe impl<T: 'static + Send + Sync> Sync for Obj<T> {}

impl<T> fmt::Debug for Obj<T>
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

impl<T: 'static + Send + Sync> Obj<T> {
    pub fn new(value: T, s: &Session) -> Self {
        let mut inner = s.gp_arena.inner.borrow_mut();

        let ptr = NonNull::from(inner.bump.alloc(value));
        inner.singles.push(ptr);

        Self {
            generation: s.gp_arena.generation,
            ptr,
        }
    }

    pub fn r(self, s: &Session) -> &T {
        assert_eq!(s.gp_arena.generation, self.generation);
        unsafe { self.ptr.as_ref() }
    }
}

// === ObjInterner === //

#[derive_where(Default)]
pub struct ObjInterner<T: 'static + Send + Sync> {
    interns: RefCell<FxHashMap<(Obj<T>, u64), ()>>,
}

impl<T: 'static + Send + Sync> fmt::Debug for ObjInterner<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ObjInterner").finish_non_exhaustive()
    }
}

impl<T> ObjInterner<T>
where
    T: 'static + Send + Sync + hash::Hash + Eq,
{
    pub fn intern(&self, ty: T, s: &Session) -> Obj<T> {
        let mut interns = self.interns.borrow_mut();
        let hash = FxBuildHasher::default().hash_one(&ty);

        match interns
            .raw_entry_mut()
            .from_hash(hash, |(other, other_hash)| {
                hash == *other_hash && &ty == other.r(s)
            }) {
            hash_map::RawEntryMut::Occupied(entry) => entry.key().0,
            hash_map::RawEntryMut::Vacant(entry) => {
                let ty = Obj::new(ty, s);
                entry.insert_with_hasher(hash, (ty, hash), (), |(_, hash)| *hash);
                ty
            }
        }
    }
}

// === LateInit === //

pub struct LateInit<T> {
    is_init: Cell<bool>,
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
            is_init: Cell::new(true),
            cell: UnsafeCell::new(MaybeUninit::new(value)),
        }
    }

    pub const fn uninit() -> Self {
        Self {
            is_init: Cell::new(false),
            cell: UnsafeCell::new(MaybeUninit::uninit()),
        }
    }

    pub fn init(me: &Self, value: T) {
        // Ensure that the cell is only initialized once.
        if me.is_init.get() {
            panic!("cannot initialize `LateInit` more than once");
        }

        // Initialize the cell.
        unsafe { *me.cell.get() = MaybeUninit::new(value) };

        // Mark the cell as initialized.
        me.is_init.set(true);
    }

    pub fn get(me: &Self) -> Option<&T> {
        me.is_init
            .get()
            .then(|| unsafe { (*me.cell.get()).assume_init_ref() })
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
        if self.is_init.get() {
            unsafe { self.cell.get_mut().assume_init_drop() }
        }
    }
}
