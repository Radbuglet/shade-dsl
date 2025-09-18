use std::{cell::RefCell, ops::Deref, sync::Arc};

use crate::base::{
    DiagCtxt,
    ir::IrArena,
    syntax::{SourceMap, SymbolInterner},
};

thread_local! {
    static SESSION_TLS: RefCell<Option<Session>> = const { RefCell::new(None) };
}

#[derive(Debug, Clone, Default)]
pub struct Session(Arc<SessionInner>);

#[derive(Debug, Default)]
pub struct SessionInner {
    pub symbols: SymbolInterner,
    pub diag: DiagCtxt,
    pub source_map: SourceMap,
    pub ir_arena: IrArena,
}

impl Session {
    #[must_use]
    pub fn bind(self) -> impl Sized {
        let old = SESSION_TLS.replace(Some(self));

        scopeguard::guard(old, move |old| {
            SESSION_TLS.set(old);
        })
    }

    pub fn fetch() -> Session {
        SESSION_TLS
            .with_borrow(|v| v.clone())
            .expect("no session was ever bound")
    }
}

impl Deref for Session {
    type Target = SessionInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
