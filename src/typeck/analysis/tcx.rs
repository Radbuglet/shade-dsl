use std::{cell::RefCell, ops::Deref, rc::Rc};

use index_vec::IndexVec;

use crate::{
    base::{
        Session,
        analysis::Memo,
        arena::{Obj, ObjInterner, ObjListInterner},
        syntax::Symbol,
    },
    typeck::{
        analysis::{IntrinsicResolver, TypeCheckFacts},
        syntax::{
            AdtInstance, BycFunction, Func, FuncInstance, Generic, MetaFuncIntrinsic,
            OwnGenericIdx, Ty, TyList, ValueArena, ValueInterner, ValuePlace,
        },
    },
    utils::hash::{FxHashMap, FxHashSet},
};

#[derive(Debug, Clone)]
pub struct TyCtxt {
    inner: Rc<TyCtxtInner>,
}

#[derive(Debug)]
pub struct TyCtxtInner {
    pub session: Session,
    pub intrinsic_resolver: IntrinsicResolver,
    pub intrinsic_cache: RefCell<FxHashMap<Symbol, ValuePlace>>,
    pub value_interner: ValueInterner,
    pub ty_interner: ObjInterner<Ty>,
    pub ty_list_interner: ObjListInterner<Obj<Ty>>,
    pub fn_interner: ObjInterner<FuncInstance>,
    pub wf_state: RefCell<WfState>,
    pub queries: Queries,
}

#[derive(Debug, Default)]
pub struct WfState {
    pub validated: FxHashSet<WfRequirement>,
    pub queue: Vec<WfRequirement>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum WfRequirement {
    TypeCheck(Obj<FuncInstance>),
    EvaluateAny(Obj<FuncInstance>),
    EvaluateType(Obj<FuncInstance>),
    EvalGeneric(Obj<Generic>, Obj<FuncInstance>),
    ValidateAdt(AdtInstance),
}

#[derive(Debug, Default)]
pub struct Queries {
    pub eval_paramless: Memo<Obj<FuncInstance>, ValuePlace>,
    pub type_check: Memo<Obj<FuncInstance>, Obj<TypeCheckFacts>>,
    pub build_bytecode: Memo<Obj<FuncInstance>, Obj<BycFunction>>,
    pub eval_intrinsic_meta_fn: Memo<(MetaFuncIntrinsic, Vec<ValuePlace>), ValuePlace>,
    pub instance_signature: Memo<Obj<FuncInstance>, (TyList, Option<Obj<Ty>>)>,
}

impl Deref for TyCtxt {
    type Target = TyCtxtInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl TyCtxt {
    pub fn new(session: Session, intrinsic_resolver: IntrinsicResolver) -> Self {
        Self {
            inner: Rc::new(TyCtxtInner {
                session,
                intrinsic_resolver,
                intrinsic_cache: RefCell::default(),
                value_interner: ValueInterner::default(),
                ty_interner: ObjInterner::default(),
                ty_list_interner: ObjListInterner::default(),
                fn_interner: ObjInterner::default(),
                wf_state: RefCell::default(),
                queries: Queries::default(),
            }),
        }
    }

    pub fn try_intern_from_scratch_arena<E>(
        &self,
        f: impl FnOnce(&mut ValueArena) -> Result<ValuePlace, E>,
    ) -> Result<ValuePlace, E> {
        let mut arena = ValueArena::default();
        let root = f(&mut arena)?;
        Ok(self.value_interner.intern(&arena, root))
    }

    pub fn intern_from_scratch_arena(
        &self,
        f: impl FnOnce(&mut ValueArena) -> ValuePlace,
    ) -> ValuePlace {
        enum Never {}

        match self.try_intern_from_scratch_arena::<Never>(|arena| Ok(f(arena))) {
            Ok(v) => v,
            Err(e) => match e {},
        }
    }

    pub fn intern_ty(&self, ty: Ty) -> Obj<Ty> {
        self.ty_interner.intern(ty, &self.session)
    }

    pub fn intern_tys(&self, ty: &[Obj<Ty>]) -> Obj<[Obj<Ty>]> {
        self.ty_list_interner.intern(ty, &self.session)
    }

    pub fn intern_fn_instance_with(
        &self,
        func: Obj<Func>,
        parent: Option<Obj<FuncInstance>>,
        generics: IndexVec<OwnGenericIdx, ValuePlace>,
    ) -> Obj<FuncInstance> {
        let s = &self.session;

        debug_assert_eq!(generics.len(), func.r(s).inner.generics.len());

        let parent = match (parent, func.r(&self.session).parent) {
            (Some(mut parent), Some(expected_parent)) => {
                while parent.r(s).func != expected_parent {
                    parent = parent.r(s).parent.unwrap();
                }

                Some(parent)
            }
            (None, None) => None,
            _ => unreachable!(),
        };

        self.fn_interner.intern(
            FuncInstance {
                func,
                parent,
                generics,
            },
            &self.session,
        )
    }

    pub fn intern_fn_instance(
        &self,
        func: Obj<Func>,
        parent: Option<Obj<FuncInstance>>,
    ) -> Obj<FuncInstance> {
        self.intern_fn_instance_with(func, parent, IndexVec::new())
    }

    pub fn queue_wf(&self, req: WfRequirement) {
        let mut state = self.wf_state.borrow_mut();

        if state.validated.insert(req) {
            state.queue.push(req);
        }
    }

    pub fn flush_wf(&self) {
        let s = &self.session;

        loop {
            let Some(req) = self.wf_state.borrow_mut().queue.pop() else {
                break;
            };

            match req {
                WfRequirement::TypeCheck(instance) => {
                    _ = self.type_check(instance);
                }
                WfRequirement::EvaluateAny(instance) => {
                    _ = self.eval_paramless(instance);
                }
                WfRequirement::EvaluateType(instance) => {
                    _ = self.eval_paramless_for_meta_ty(instance);
                }
                WfRequirement::ValidateAdt(instance) => {
                    let lexical = instance.adt.r(s);

                    for field in &lexical.fields {
                        _ = self.eval_paramless_for_meta_ty(
                            self.intern_fn_instance(field.ty, Some(instance.owner)),
                        );
                    }

                    for member in &lexical.members {
                        _ = self.eval_paramless(
                            self.intern_fn_instance(member.init, Some(instance.owner)),
                        );
                    }
                }
                WfRequirement::EvalGeneric(generic, instance) => {
                    _ = self.eval_generic_ensuring_conformance(generic, instance);
                }
            }
        }
    }
}
