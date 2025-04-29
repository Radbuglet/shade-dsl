use std::{cell::Cell, ptr::NonNull};

use crate::semantic::syntax::{BoundValue, Ty, TyAdtMember, TyKind, Value, ValueKind};

use super::{Interner, ListInterner, Symbol, SymbolInterner};

pub type Gcx<'gcx> = &'gcx GcxOwned<'gcx>;

#[derive(Default)]
pub struct GcxOwned<'gcx> {
    pub arena: bumpalo::Bump,
    pub symbols: SymbolInterner,

    pub type_interner: Interner<'gcx, TyKind<'gcx>>,
    pub type_list_interner: ListInterner<'gcx, Ty<'gcx>>,
    pub value_interner: Interner<'gcx, ValueKind<'gcx>>,
    pub value_list_interner: ListInterner<'gcx, Value<'gcx>>,
    pub bound_value_list_interner: ListInterner<'gcx, BoundValue<'gcx>>,
    pub symbol_list_interner: ListInterner<'gcx, Symbol>,
    pub member_list_interner: ListInterner<'gcx, TyAdtMember<'gcx>>,
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
        self.arena.alloc(val)
    }

    pub fn alloc_slice<T: Clone>(&'gcx self, val: &[T]) -> &'gcx [T] {
        self.arena.alloc_slice_clone(val)
    }
}
