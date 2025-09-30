use rustc_hash::FxHashMap;

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::LiteralKind,
    typeck::{
        analysis::{WfRequirement, tcx::TyCtxt},
        syntax::{
            AnyName, CheckTy, Expr, ExprKind, FuncInstance, Generic, ScalarKind, Ty, TyList,
            ValuePlace,
        },
    },
};

impl TyCtxt {
    pub fn type_check(&self, instance: Obj<FuncInstance>) -> Result<(), ErrorGuaranteed> {
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
            let (args, expected_ret) = self.instance_signature(instance)?;

            // Type-check the body with all expectations.
            let mut cx = CheckCx {
                tcx: self,
                instance,
                args: args.r(s),
                erroneous: None,
                facts: TypeCheckFacts::default(),
            };

            cx.check_expr(func.body, expected_ret);

            if let Some(err) = cx.erroneous {
                return Err(err);
            }

            Ok(())
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
    args: &'a [Obj<Ty>],
    facts: TypeCheckFacts,
    erroneous: Option<ErrorGuaranteed>,
}

impl CheckCx<'_> {
    fn check_expr(&mut self, expr: Obj<Expr>, expected_ty: Option<Obj<Ty>>) -> CheckTy {
        let s = &self.tcx.session;

        let actual_ty = match &expr.r(s).kind {
            ExprKind::Name(name) => match name {
                AnyName::Const(target) => {
                    let instance = self.tcx.intern_fn_instance(*target, Some(self.instance));

                    let ty = match self.tcx.ty_of_paramless_fn_val(instance) {
                        Ok(v) => v,
                        Err(err) => {
                            self.erroneous = Some(err);
                            return CheckTy::Unknown;
                        }
                    };

                    CheckTy::Regular(ty)
                }
                AnyName::Generic(ty) => {
                    let value = match self
                        .tcx
                        .eval_generic_ensuring_conformance(*ty, self.instance)
                    {
                        Ok(v) => v,
                        Err(err) => {
                            self.erroneous = Some(err);
                            return CheckTy::Unknown;
                        }
                    };

                    CheckTy::Regular(self.tcx.value_interner.read(value).ty)
                }
                AnyName::Local(obj) => todo!(),
            },
            ExprKind::Block(obj) => todo!(),
            ExprKind::Lit(kind) => match kind {
                LiteralKind::BoolLit(_) => {
                    CheckTy::Regular(self.tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)))
                }
                LiteralKind::StrLit(token_str_lit) => todo!(),
                LiteralKind::CharLit(token_char_lit) => todo!(),
                LiteralKind::NumLit(token_num_lit) => todo!(),
            },
            ExprKind::BinOp(bin_op_kind, obj, obj1) => todo!(),
            ExprKind::Call(obj, objs) => todo!(),
            ExprKind::Destructure(obj, obj1) => todo!(),
            ExprKind::Match(expr_match) => todo!(),
            ExprKind::Adt(_adt) => {
                // WF checks are deferred until construction.
                CheckTy::Regular(self.tcx.intern_ty(Ty::MetaTy))
            }
            ExprKind::Func(func) => {
                if func.r(s).inner.generics.is_empty() {
                    let instance = self.tcx.intern_fn_instance(*func, Some(self.instance));

                    self.tcx.queue_wf(WfRequirement::TypeCheck(instance));

                    let ty = match self.tcx.instance_fn_ty(instance) {
                        Ok(v) => v,
                        Err(e) => {
                            self.erroneous = Some(e);
                            return CheckTy::Unknown;
                        }
                    };

                    CheckTy::Regular(ty)
                } else {
                    CheckTy::Regular(self.tcx.intern_ty(Ty::MetaFunc))
                }
            }
            ExprKind::Instantiate(callee, args) => {
                self.check_expr(*callee, Some(self.tcx.intern_ty(Ty::MetaFunc)));

                for arg in args {
                    self.check_expr(*arg, None);
                }

                CheckTy::Unknown
            }
            ExprKind::Intrinsic(symbol) => todo!(),
            ExprKind::Error(error_guaranteed) => todo!(),
            ExprKind::Placeholder => todo!(),
        };

        if let Some(expected_ty) = expected_ty
            && actual_ty != CheckTy::Regular(expected_ty)
        {
            todo!();
        }

        self.facts.expr_types.insert(expr, actual_ty);

        actual_ty
    }
}

#[derive(Debug, Default)]
pub struct TypeCheckFacts {
    pub expr_types: FxHashMap<Obj<Expr>, CheckTy>,
}
