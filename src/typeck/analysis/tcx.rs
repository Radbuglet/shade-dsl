use std::{ops::Deref, rc::Rc};

use index_vec::IndexVec;

use crate::{
    base::{
        Session,
        analysis::Memo,
        arena::{Obj, ObjInterner},
    },
    typeck::syntax::{BycFunction, Func, FuncInstance, Ty, ValueInterner, ValuePlace},
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
pub struct Queries {
    pub eval_paramless: Memo<Obj<FuncInstance>, ValuePlace>,
    pub type_check: Memo<Obj<FuncInstance>, ()>,
    pub build_bytecode: Memo<Obj<FuncInstance>, Obj<BycFunction>>,
}

impl Deref for TyCtxt {
    type Target = TyCtxtInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

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

    pub fn intern_fn_instance(
        &self,
        func: Obj<Func>,
        parent: Option<Obj<FuncInstance>>,
    ) -> Obj<FuncInstance> {
        self.fn_interner.intern(
            FuncInstance {
                func,
                parent,
                generics: IndexVec::new(),
            },
            &self.session,
        )
    }
}
