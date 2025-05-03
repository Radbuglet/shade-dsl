use std::{cell::Cell, hash, ptr::NonNull};

use crate::semantic::syntax::{BoundValue, Ty, TyAdtMember, TyKind, Value, ValueInner};

use super::{
    DiagCtxt, Intern, InternList, Interner, ListInterner, SourceMap, Symbol, SymbolInterner,
};

pub type Gcx<'gcx> = &'gcx GcxOwned<'gcx>;

pub struct GcxOwned<'gcx> {
    pub bump: bumpalo::Bump,
    pub diag: DiagCtxt,
    pub symbols: SymbolInterner,
    pub spans: SourceMap,
    pub interners: GcxInterners<'gcx>,

    pub interned: PreInterned<'gcx>,
}

pub struct PreInterned<'gcx> {
    pub unit: Ty<'gcx>,
    pub meta_fn: Ty<'gcx>,
    pub meta_ty: Ty<'gcx>,
    pub meta_producer_fn: Ty<'gcx>,
}

thread_local! {
    static CURR_GCX: Cell<Option<NonNull<()>>> = const { Cell::new(None) };
}

impl<'gcx> GcxOwned<'gcx> {
    pub fn provide_tls<R>(&'gcx self, f: impl FnOnce(Gcx<'_>) -> R) -> R {
        let _guard = scopeguard::guard(CURR_GCX.take(), |old| {
            CURR_GCX.set(old);
        });

        CURR_GCX.set(Some(NonNull::from(self).cast()));
        f(self)
    }

    pub fn fetch_tls<R>(f: impl FnOnce(Gcx<'_>) -> R) -> R {
        f(unsafe {
            CURR_GCX
                .get()
                .expect("no gcx bound to the current thread")
                .cast::<GcxOwned<'_>>()
                .as_ref()
        })
    }

    pub fn alloc<T>(&'gcx self, val: T) -> &'gcx T {
        self.bump.alloc(val)
    }

    pub fn alloc_slice<T: Clone>(&'gcx self, val: &[T]) -> &'gcx [T] {
        self.bump.alloc_slice_clone(val)
    }

    pub fn intern<T: Internable<'gcx>>(&'gcx self, value: T) -> Intern<'gcx, T> {
        T::fetch(&self.interners).intern(&self.bump, value)
    }

    pub fn intern_slice<T: ListInternable<'gcx>>(&'gcx self, value: &[T]) -> InternList<'gcx, T> {
        T::fetch(&self.interners).intern(&self.bump, value)
    }

    pub fn intern_iter<T>(&'gcx self, iter: impl IntoIterator<Item = T>) -> InternList<'gcx, T>
    where
        T: ListInternable<'gcx>,
    {
        T::fetch(&self.interners).intern_iter(&self.bump, iter)
    }
}

// === Interner Bundles === //

pub trait Internable<'gcx>: Sized + hash::Hash + Eq {
    fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx Interner<'gcx, Self>;
}

pub trait ListInternable<'gcx>: Sized + hash::Hash + Eq + Clone {
    fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx ListInterner<'gcx, Self>;
}

macro_rules! define_interners {
    (
        single {
            $($single_name:ident: $single_ty:ty),*
            $(,)?
        }

        list {
            $($list_name:ident: $list_ty:ty),*
            $(,)?
        }
    ) => {
        #[derive(Default)]
        pub struct GcxInterners<'gcx> {
            $(pub $single_name: Interner<'gcx, $single_ty>,)*
            $(pub $list_name: ListInterner<'gcx, $list_ty>,)*
        }

        $(
            impl<'gcx> Internable<'gcx> for $single_ty {
                fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx Interner<'gcx, Self> {
                    &interners.$single_name
                }
            }
        )*

        $(
            impl<'gcx> ListInternable<'gcx> for $list_ty {
                fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx ListInterner<'gcx, Self> {
                    &interners.$list_name
                }
            }
        )*
    };
}

define_interners! {
    single {
        type_interner: TyKind<'gcx>,
        value_interner: ValueInner<'gcx>,
    }

    list {
        type_list_interner: Ty<'gcx>,
        value_list_interner: Value<'gcx>,
        bound_value_list_interner: BoundValue<'gcx>,
        symbol_list_interner: Symbol,
        member_list_interner: TyAdtMember<'gcx>,
    }
}
