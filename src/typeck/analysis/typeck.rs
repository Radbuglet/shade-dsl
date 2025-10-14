use hashbrown::hash_map;

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::{LiteralKind, Mutability},
    typeck::{
        analysis::{WfRequirement, tcx::TyCtxt},
        syntax::{
            AdtInstance, AnyName, Expr, ExprKind, FuncInstance, Generic, Local, Pat, PatKind,
            ScalarKind, Stmt, TaggedTy, Ty, TyList, TyTag, ValuePlace,
        },
    },
    utils::hash::FxHashMap,
};

impl TyCtxt {
    pub fn type_check(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<Obj<TypeCheckFacts>, ErrorGuaranteed> {
        let s = &self.session;

        self.queries.type_check.compute(instance, |_| {
            let func = &*instance.r(s).func.r(s).inner;

            // Ensure that all generics have the right type and that constants evaluate to something
            // valid, even if they go unused.
            for &generic in &func.generics {
                self.queue_wf(super::WfRequirement::EvalGeneric(generic, instance));
            }

            for &cst in &func.consts {
                self.queue_wf(super::WfRequirement::EvaluateAny(
                    self.intern_fn_instance(cst, Some(instance)),
                ));
            }

            // Determine the function's signature.
            let (expected_args, expected_ret) = self.instance_signature(instance)?;

            // Type-check the body with all expectations.
            let mut cx = CheckCx {
                tcx: self,
                instance,
                erroneous: None,
                facts: TypeCheckFacts {
                    expr_types: FxHashMap::default(),
                    local_types: FxHashMap::default(),
                    index_exprs: FxHashMap::default(),
                    return_ty: self.intern_ty(Ty::Never), // (placeholder)
                },
            };

            if let Some(params) = &func.params {
                for (param, expected_arg) in params.iter().zip(expected_args.r(s)) {
                    cx.check_pat(param.binding, *expected_arg);
                }
            }

            cx.facts.return_ty = cx.check_expr(func.body, expected_ret).ty;

            if let Some(err) = cx.erroneous {
                return Err(err);
            }

            Ok(Obj::new(cx.facts, s))
        })
    }

    pub fn instance_fn_ty(&self, instance: Obj<FuncInstance>) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let (args, ret_ty) = self.instance_signature(instance)?;
        Ok(self.intern_ty(Ty::Func(args, ret_ty.unwrap())))
    }

    pub fn instance_signature(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<(TyList, Option<Obj<Ty>>), ErrorGuaranteed> {
        let s = &self.session;

        self.queries.instance_signature.compute(instance, |_| {
            let func = &*instance.r(s).func.r(s).inner;

            let mut arg_tys = Vec::new();

            if let Some(args) = &func.params {
                for arg in args {
                    arg_tys.push(self.eval_paramless_for_meta_ty(
                        self.intern_fn_instance(arg.ty, Some(instance)),
                    )?);
                }
            }

            let ret_ty = match func.return_type {
                Some(ty) => Some(
                    self.eval_paramless_for_meta_ty(self.intern_fn_instance(ty, Some(instance)))?,
                ),
                None => None,
            };

            Ok((Obj::new_slice(&arg_tys, s), ret_ty))
        })
    }

    pub fn eval_generic_ensuring_conformance(
        &self,
        generic: Obj<Generic>,
        instance: Obj<FuncInstance>,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        let s = &self.session;

        let generic = generic.r(s);
        let mut instance_obj = instance;
        let mut instance = instance.r(s);

        while generic.owner != instance.func {
            instance_obj = instance.parent.unwrap();
            instance = instance_obj.r(s);
        }

        let actual_value = instance.generics[generic.idx];
        let actual_ty = self.value_interner.read(actual_value).ty;

        let expected_ty = self
            .eval_paramless_for_meta_ty(self.intern_fn_instance(generic.ty, Some(instance_obj)))?;

        if expected_ty != actual_ty {
            todo!();
        }

        Ok(actual_value)
    }
}

struct CheckCx<'a> {
    tcx: &'a TyCtxt,
    instance: Obj<FuncInstance>,
    facts: TypeCheckFacts,
    erroneous: Option<ErrorGuaranteed>,
}

impl CheckCx<'_> {
    fn err(&mut self, err: ErrorGuaranteed) -> Obj<Ty> {
        self.erroneous = Some(err);
        self.tcx.intern_ty(Ty::Error(err))
    }

    fn check_pat(&mut self, pat: Obj<Pat>, ty: Obj<Ty>) {
        let s = &self.tcx.session;

        match &pat.r(s).kind {
            PatKind::Hole => {
                // (trivially satisfied)
            }
            PatKind::Name(local) => match self.facts.local_types.entry(*local) {
                hash_map::Entry::Occupied(entry) => {
                    if *entry.get() != ty {
                        todo!()
                    }
                }
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(ty);
                }
            },
            PatKind::Tuple(pat) => {
                let Ty::Tuple(ty) = ty.r(s) else {
                    todo!();
                };

                if pat.len() != ty.r(s).len() {
                    todo!();
                }

                for (&pat, &ty) in pat.iter().zip(ty.r(s)) {
                    self.check_pat(pat, ty);
                }
            }
            PatKind::Error(err) => {
                self.erroneous = Some(*err);
            }
        }
    }

    fn check_expr(&mut self, expr: Obj<Expr>, expected_ty: Option<Obj<Ty>>) -> TaggedTy {
        let s = &self.tcx.session;

        let actual_ty = self.check_expr_inner(expr);

        if let Some(expected_ty) = expected_ty
            && actual_ty.ty != expected_ty
        {
            todo!(
                "type mismatch at {} in {expr:#?}: {expected_ty:#?}, {actual_ty:#?}",
                expr.r(s).span
            );
        }

        self.facts.expr_types.insert(expr, actual_ty.ty);

        actual_ty
    }

    fn check_expr_inner(&mut self, expr: Obj<Expr>) -> TaggedTy {
        let s = &self.tcx.session;

        match &expr.r(s).kind {
            ExprKind::Name(name) => match name {
                AnyName::Const(target) => {
                    let instance = self.tcx.intern_fn_instance(*target, Some(self.instance));

                    let (Ok(ty) | Err(ty)) = self
                        .tcx
                        .tagged_ty_for_eval_paramless_unwrap_any(instance)
                        .map_err(|v| TaggedTy::untagged(self.err(v)));

                    ty
                }
                AnyName::Generic(ty) => {
                    match self
                        .tcx
                        .eval_generic_ensuring_conformance(*ty, self.instance)
                    {
                        Ok(value) => self.tcx.tagged_ty_from_intern(value),
                        Err(err) => TaggedTy::untagged(self.err(err)),
                    }
                }
                AnyName::Local(local) => {
                    let Some(ty) = self.facts.local_types.get(local) else {
                        todo!();
                    };

                    TaggedTy::untagged(*ty)
                }
            },
            ExprKind::Block(block) => {
                for stmt in &block.r(s).stmts {
                    match stmt {
                        Stmt::Live(_) => {
                            // (nothing to do)
                        }
                        Stmt::Expr(expr) => {
                            self.check_expr(
                                *expr,
                                Some(self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[])))),
                            );
                        }
                    }
                }

                if let Some(last_expr) = block.r(s).last_expr {
                    self.check_expr(last_expr, None)
                } else {
                    TaggedTy::untagged(self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[]))))
                }
            }
            ExprKind::Lit(kind) => match kind {
                LiteralKind::BoolLit(_) => {
                    TaggedTy::untagged(self.tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)))
                }
                LiteralKind::StrLit(_) => TaggedTy::untagged(self.tcx.intern_ty(Ty::MetaString)),
                LiteralKind::CharLit(token_char_lit) => todo!(),
                LiteralKind::NumLit(lit) => todo!(),
            },
            ExprKind::BinOp(bin_op_kind, obj, obj1) => todo!(),
            ExprKind::Call(callee, args) => {
                let callee = self.check_expr(*callee, None);

                let Ty::Func(expected_args, expected_rv) = callee.ty.r(s) else {
                    todo!()
                };

                if args.len() != expected_args.r(s).len() {
                    todo!()
                }

                for (&arg, &expected_ty) in args.iter().zip(expected_args.r(s)) {
                    self.check_expr(arg, Some(expected_ty));
                }

                TaggedTy::untagged(*expected_rv)
            }
            ExprKind::Destructure(pat, init) => {
                let init_ty = self.check_expr(*init, None).ty;
                self.check_pat(*pat, init_ty);

                TaggedTy::untagged(self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[]))))
            }
            ExprKind::Assign(lhs, rhs) => {
                let muta = self.check_place_mutability(*lhs);

                if muta.is_not() {
                    todo!("not mutable");
                }

                let lhs_ty = self.check_expr(*lhs, None);
                self.check_expr(*rhs, Some(lhs_ty.ty));

                TaggedTy::untagged(self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[]))))
            }
            ExprKind::NamedIndex(lhs, name) => {
                let lhs_ty = self.check_expr(*lhs, None);

                let (index_kind, index_result) = match (lhs_ty.ty.r(s), lhs_ty.tag) {
                    (Ty::MetaTy, TyTag::MetaTyKnown(ty)) => {
                        let Ty::Adt(adt) = ty.r(s) else {
                            todo!();
                        };

                        let Some(member) = adt.adt.r(s).members.iter().find(|v| v.name == *name)
                        else {
                            todo!("no such member");
                        };

                        if !member.is_public {
                            'vis_check: {
                                let module = member.init.r(s).parent.unwrap();

                                let mut iter = Some(self.instance.r(s).func);

                                while let Some(curr) = iter {
                                    if curr == module {
                                        break 'vis_check;
                                    }

                                    iter = curr.r(s).parent;
                                }

                                todo!("not accessible");
                            }
                        }

                        let member = self.tcx.intern_fn_instance(member.init, Some(adt.owner));

                        let ty = self
                            .tcx
                            .tagged_ty_for_eval_paramless_unwrap_any(member)
                            .unwrap_or_else(|err| TaggedTy::untagged(self.err(err)));

                        (IndexKind::ConstMember(member), ty)
                    }
                    // Ty::Pointer(obj) => todo!(),
                    // Ty::Tuple(obj) => todo!(),
                    // Ty::Adt(adt_instance) => todo!(),
                    // Ty::Error(error_guaranteed) => todo!(),
                    _ => todo!(),
                };

                self.facts.index_exprs.insert(expr, index_kind);

                index_result
            }
            ExprKind::Match(expr_match) => todo!(),
            ExprKind::Adt(adt) => {
                self.tcx.queue_wf(WfRequirement::ValidateAdt(AdtInstance {
                    owner: self.instance,
                    adt: *adt,
                }));

                let type_represented = self.tcx.intern_ty(Ty::Adt(AdtInstance {
                    owner: self.instance,
                    adt: *adt,
                }));

                TaggedTy::new(
                    self.tcx.intern_ty(Ty::MetaTy),
                    TyTag::MetaTyKnown(type_represented),
                )
            }
            ExprKind::Func(func) => {
                if func.r(s).inner.generics.is_empty() {
                    let instance = self.tcx.intern_fn_instance(*func, Some(self.instance));

                    self.tcx.queue_wf(WfRequirement::TypeCheck(instance));

                    let (Ok(ty) | Err(ty)) =
                        self.tcx.instance_fn_ty(instance).map_err(|v| self.err(v));

                    TaggedTy::untagged(ty)
                } else {
                    TaggedTy::untagged(self.tcx.intern_ty(Ty::MetaFunc))
                }
            }
            ExprKind::Instantiate(callee, args) => {
                self.check_expr(*callee, Some(self.tcx.intern_ty(Ty::MetaFunc)));

                for arg in args {
                    self.check_expr(*arg, None);
                }

                TaggedTy::untagged(self.tcx.intern_ty(Ty::MetaAny))
            }
            ExprKind::Intrinsic(name) => {
                let Some(intrinsic) = self.tcx.resolve_intrinsic(*name) else {
                    todo!();
                };

                self.tcx.tagged_ty_from_intern(intrinsic)
            }
            ExprKind::NewTuple(fields) => {
                let tys = fields
                    .iter()
                    .map(|expr| self.check_expr(*expr, None).ty)
                    .collect::<Vec<_>>();

                TaggedTy::untagged(self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&tys))))
            }
            ExprKind::NewTupleType(_) => TaggedTy::untagged(self.tcx.intern_ty(Ty::MetaTy)),
            ExprKind::Error(err) => TaggedTy::untagged(self.err(*err)),
        }
    }

    fn check_place_mutability(&mut self, expr: Obj<Expr>) -> Mutability {
        let s = &self.tcx.session;

        match &expr.r(s).kind {
            ExprKind::Name(AnyName::Local(local)) => local.r(s).muta,
            ExprKind::Error(_) => Mutability::Mut,
            _ => todo!("cannot assign"),
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckFacts {
    pub expr_types: FxHashMap<Obj<Expr>, Obj<Ty>>,
    pub local_types: FxHashMap<Obj<Local>, Obj<Ty>>,
    pub index_exprs: FxHashMap<Obj<Expr>, IndexKind>,
    pub return_ty: Obj<Ty>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum IndexKind {
    AdtField(u32),
    TupleField(u32),
    ConstMember(Obj<FuncInstance>),
}
