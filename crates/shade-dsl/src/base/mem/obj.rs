use std::{any::TypeId, cell::RefCell, fmt};

use ctx2d_utils::hash::FxHashSet;
use late_struct::LateField;
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

pub(crate) fn guard_entity_reentrancy_checks() -> impl Sized {
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
        super::{Component, Handle},
        crate::base::World,
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

use crate::base::{World, WorldDebug};
