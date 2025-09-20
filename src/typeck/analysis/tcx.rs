use std::{ops::Deref, rc::Rc};

use crate::{
    base::{Session, arena::ObjInterner},
    typeck::syntax::{FuncInstance, Ty, ValueInterner},
};

#[derive(Debug, Clone)]
pub struct TyCtxt {
    inner: Rc<TyCtxtInner>,
}

#[derive(Debug)]
pub struct TyCtxtInner {
    pub session: Session,
    pub value_interner: ValueInterner,
    pub ty_interner: ObjInterner<Ty>,
    pub fn_interner: ObjInterner<FuncInstance>,
    pub queries: Queries,
}

#[derive(Debug, Default)]
pub struct Queries {}

impl TyCtxt {
    pub fn new(session: Session) -> Self {
        Self {
            inner: Rc::new(TyCtxtInner {
                session,
                value_interner: ValueInterner::default(),
                ty_interner: ObjInterner::default(),
                fn_interner: ObjInterner::default(),
                queries: Queries::default(),
            }),
        }
    }
}

impl Deref for TyCtxt {
    type Target = TyCtxtInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
