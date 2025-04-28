use std::{cell::Cell, ptr::NonNull};

use super::SymbolInterner;

pub type Gcx<'gcx> = &'gcx GcxOwned<'gcx>;

#[derive(Debug, Default)]
pub struct GcxOwned<'gcx> {
    _ty: [&'gcx (); 0],
    pub arena: bumpalo::Bump,
    pub symbols: SymbolInterner,
}

thread_local! {
    static CURR_GCX: Cell<Option<NonNull<()>>> = const { Cell::new(None) };
}

impl<'gcx> GcxOwned<'gcx> {
    pub fn provide_tls<R>(&self, f: impl FnOnce() -> R) -> R {
        let _guard = scopeguard::guard(CURR_GCX.take(), |old| {
            CURR_GCX.set(old);
        });

        CURR_GCX.set(Some(NonNull::from(self).cast()));
        f()
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
}
