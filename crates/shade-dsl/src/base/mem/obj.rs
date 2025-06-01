use std::{
    any::TypeId,
    cell::{self, Cell, RefCell},
    fmt, mem,
    ptr::NonNull,
};

use ctx2d_utils::hash::FxHashSet;
use late_struct::{LateField, LateInstance, late_struct};
use thunderdome::{Arena, Index};

// === Format Reentrancy === //

#[derive(Debug, Default)]
struct FmtReentrancyState {
    depth: u32,
    objects: FxHashSet<(TypeId, Index)>,
}

thread_local! {
    static FMT_REENTRANCY: RefCell<Option<FmtReentrancyState>> = const { RefCell::new(None) };
}

fn guard_entity_reentrancy_checks() -> impl Sized {
    FMT_REENTRANCY.with(|v| {
        let mut v = v.borrow_mut();

        let v = v.get_or_insert_default();
        v.depth += 1;
    });

    scopeguard::guard((), |()| {
        FMT_REENTRANCY.with(|v| {
            let mut v = v.borrow_mut();
            let v_inner = v.as_mut().unwrap();
            v_inner.depth -= 1;

            if v_inner.depth == 0 {
                *v = None;
            }
        });
    })
}

fn can_format_obj<T: Handle>(obj: T) -> bool {
    FMT_REENTRANCY.with(|v| {
        let mut v = v.borrow_mut();
        let v = v
            .as_mut()
            .expect("cannot call `can_format_obj` without a bound immutable world");

        v.objects.insert((TypeId::of::<T::Component>(), obj.raw()))
    })
}

// === World === //

pub type W<'a> = &'a mut World;
pub type Wr<'a> = &'a World;

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct World {
    pub raw: LateInstance<Self>,
}

thread_local! {
    static WORLD_TLS: Cell<Option<NonNull<WorldTlsState>> >= const { Cell::new(None) };
}

struct WorldTlsState(RefCell<NonNull<World>>);

late_struct!(World);

impl World {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn wrap_ref(raw: &LateInstance<Self>) -> &Self {
        unsafe { &*(raw as *const LateInstance<Self> as *const Self) }
    }

    pub fn wrap_mut(raw: &mut LateInstance<Self>) -> &mut Self {
        unsafe { &mut *(raw as *mut LateInstance<Self> as *mut Self) }
    }

    pub fn resource<F: LateField<Self>>(&self) -> &F::Value {
        self.raw.get::<F>()
    }

    pub fn resource_mut<F: LateField<Self>>(&mut self) -> &mut F::Value {
        self.raw.get_mut::<F>()
    }

    pub fn resources_two<F, G>(&mut self) -> (&mut F::Value, &mut G::Value)
    where
        F: LateField<Self>,
        G: LateField<Self>,
    {
        self.raw.get_two::<F, G>()
    }

    pub fn resource_ptr<F: LateField<Self>>(&self) -> NonNull<F::Value> {
        self.raw.get_ptr::<F>()
    }

    fn provide_tls_state<R>(state: WorldTlsState, f: impl FnOnce() -> R) -> R {
        let old = WORLD_TLS.replace(Some(NonNull::from(&state)));
        let _restore_guard = scopeguard::guard(old, |old| {
            WORLD_TLS.set(old);
        });

        f()
    }

    fn try_fetch_tls_state<R>(f: impl FnOnce(Option<&WorldTlsState>) -> R) -> R {
        f(WORLD_TLS.get().map(|v| unsafe { v.as_ref() }))
    }

    fn fetch_tls_state<R>(f: impl FnOnce(&WorldTlsState) -> R) -> R {
        Self::try_fetch_tls_state(|state| f(state.expect("no world provided")))
    }

    pub fn provide_tls_ref<R>(&self, f: impl FnOnce() -> R) -> R {
        let new_state = WorldTlsState(RefCell::new(NonNull::from(self)));
        mem::forget(new_state.0.borrow()); // (ensure that the cell cannot be borrowed mutably)

        Self::provide_tls_state(new_state, f)
    }

    pub fn provide_tls_mut<R>(&mut self, f: impl FnOnce() -> R) -> R {
        let new_state = WorldTlsState(RefCell::new(NonNull::from(self)));
        // (this cell can be borrowed both mutably and immutably)

        Self::provide_tls_state(new_state, f)
    }

    pub fn try_fetch_tls_ref<R>(
        f: impl FnOnce(Option<Result<&World, cell::BorrowError>>) -> R,
    ) -> R {
        Self::try_fetch_tls_state(|state| {
            f(state.map(|state| state.0.try_borrow().map(|v| unsafe { v.as_ref() })))
        })
    }

    pub fn try_fetch_tls_mut<R>(
        f: impl FnOnce(Option<Result<&mut World, cell::BorrowMutError>>) -> R,
    ) -> R {
        Self::try_fetch_tls_state(|state| {
            f(state.map(|state| state.0.try_borrow_mut().map(|mut v| unsafe { v.as_mut() })))
        })
    }

    pub fn fetch_tls_ref<R>(f: impl FnOnce(&World) -> R) -> R {
        Self::fetch_tls_state(|state| f(unsafe { state.0.borrow().as_ref() }))
    }

    pub fn fetch_tls_mut<R>(f: impl FnOnce(&mut World) -> R) -> R {
        Self::fetch_tls_state(|state| f(unsafe { state.0.borrow_mut().as_mut() }))
    }
}

// === WorldDebug === //

impl World {
    pub fn debug<T>(&self, target: T) -> WorldDebug<'_, T> {
        WorldDebug {
            world: self,
            target,
        }
    }
}

#[derive(Copy, Clone)]
pub struct WorldDebug<'a, T> {
    pub world: &'a World,
    pub target: T,
}

impl<T: fmt::Debug> fmt::Debug for WorldDebug<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _guard = guard_entity_reentrancy_checks();
        self.world.provide_tls_ref(|| self.target.fmt(f))
    }
}

// === Obj === //

pub type Obj<T> = <T as Component>::Handle;
pub type Val<T> = <T as Handle>::Component;

pub trait Component: 'static + Sized + fmt::Debug + LateField<World, Value = Arena<Self>> {
    type Handle: Handle<Component = Self>;
}

pub trait Handle: Sized + 'static + fmt::Debug + Copy + Eq + Ord {
    type Component: Component<Handle = Self>;

    const DANGLING: Self;

    fn wrap_raw(index: Index) -> Self;

    fn raw(self) -> Index;

    fn destroy(self, w: &mut World) {
        w.resource_mut::<Self::Component>().remove(self.raw());
    }

    fn is_alive(self, w: &World) -> bool {
        w.resource::<Self::Component>().contains(self.raw())
    }

    fn try_get(self, w: &World) -> Option<&Self::Component> {
        w.resource::<Self::Component>().get(self.raw())
    }

    fn try_get_mut(self, w: &mut World) -> Option<&mut Self::Component> {
        w.resource_mut::<Self::Component>().get_mut(self.raw())
    }

    fn get(self, w: &World) -> &Self::Component {
        &w.resource::<Self::Component>()[self.raw()]
    }

    fn get_mut(self, w: &mut World) -> &mut Self::Component {
        &mut w.resource_mut::<Self::Component>()[self.raw()]
    }

    fn r(self, w: &World) -> &Self::Component {
        self.get(w)
    }

    fn m(self, w: &mut World) -> &mut Self::Component {
        self.get_mut(w)
    }

    fn debug(self, w: &World) -> WorldDebug<'_, Self> {
        w.debug(self)
    }
}

#[doc(hidden)]
pub mod component_internals {
    use super::can_format_obj;

    pub use {
        super::{Component, Handle, World},
        late_struct::late_field,
        paste::paste,
        std::fmt,
        thunderdome::{Arena, Index},
    };

    pub fn format_obj<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        let index = handle.raw().to_bits();

        World::try_fetch_tls_ref(|world| {
            if let Some(Ok(world)) = world.filter(|_| can_format_obj::<T>(handle)) {
                let storage = world.resource::<T::Component>();

                if let Some(value) = storage.get(handle.raw()) {
                    f.debug_tuple("Obj")
                        .field(&format_args!("0x{index:x}"))
                        .field(value)
                        .finish()
                } else {
                    struct Dead;
                    impl fmt::Debug for Dead {
                        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                            f.write_str("<dead>")
                        }
                    }
                    f.debug_tuple("Obj")
                        .field(&format_args!("0x{index:x}"))
                        .field(&Dead)
                        .finish()
                }
            } else {
                f.debug_tuple("Obj")
                    .field(&format_args!("0x{index:x}"))
                    .finish()
            }
        })
    }
}

#[macro_export]
macro_rules! component {
    ($($name:ident),*$(,)?) => {$(
        $crate::base::mem::component_internals::paste! {
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
            pub struct [<Obj $name>] {
                pub raw: $crate::base::mem::component_internals::Index,
            }

            impl $crate::base::mem::component_internals::fmt::Debug for [<Obj $name>] {
                fn fmt(
                    &self,
                    f: &mut $crate::base::mem::component_internals::fmt::Formatter<'_>,
                ) -> $crate::base::mem::component_internals::fmt::Result {
                    $crate::base::mem::component_internals::format_obj(f, *self)
                }
            }

            impl [<Obj $name>] {
                pub const DANGLING: Self =
                    Self::wrap_raw($crate::base::mem::component_internals::Index::DANGLING);

                pub const fn wrap_raw(raw: $crate::base::mem::component_internals::Index) -> Self {
                    Self { raw }
                }

                pub const fn raw(self) -> $crate::base::mem::component_internals::Index {
                    self.raw
                }
            }

            impl $crate::base::mem::component_internals::Component for $name {
                type Handle = [<Obj $name>];
            }

            impl $crate::base::mem::component_internals::Handle for [<Obj $name>] {
                type Component = $name;

                const DANGLING: Self = Self::DANGLING;

                fn wrap_raw(index: $crate::base::mem::component_internals::Index) -> Self {
                    Self::wrap_raw(index)
                }

                fn raw(self) -> $crate::base::mem::component_internals::Index {
                    self.raw()
                }
            }

            $crate::base::mem::component_internals::late_field!(
                $name [$crate::base::mem::component_internals::World]
                    => $crate::base::mem::component_internals::Arena<$name>
            );
        }
    )*};
}

pub use component;
