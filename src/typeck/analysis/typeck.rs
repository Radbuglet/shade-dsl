use hashbrown::hash_map;

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::{LiteralKind, Mutability},
    typeck::{
        analysis::{WfRequirement, tcx::TyCtxt},
        syntax::{
            AdtInstance, AnyFuncValue, AnyName, Expr, ExprKind, FuncInstance, Generic, Local, Pat,
            PatKind, ScalarKind, Stmt, Ty, TyList, ValueArena, ValueArenaLike, ValueKind,
            ValuePlace,
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
                    expr_types_pre_coerce: FxHashMap::default(),
                    local_types: FxHashMap::default(),
                    index_exprs: FxHashMap::default(),
                    call_exprs: FxHashMap::default(),
                    coercions: FxHashMap::default(),
                    return_ty: self.intern_ty(Ty::Never), // (placeholder)
                },
            };

            if let Some(params) = &func.params {
                for (param, expected_arg) in params.iter().zip(expected_args.r(s)) {
                    cx.check_pat(param.binding, *expected_arg);
                }
            }

            cx.facts.return_ty = cx.check_expr(func.body, expected_ret);

            if let Some(err) = cx.erroneous {
                return Err(err);
            }

            Ok(Obj::new(cx.facts, s))
        })
    }

    pub fn any_func_value_signature(
        &self,
        func: AnyFuncValue,
    ) -> Result<(TyList, Option<Obj<Ty>>), ErrorGuaranteed> {
        match func {
            AnyFuncValue::Intrinsic(intrinsic) => {
                let (args, rv) = intrinsic.signature(self);
                Ok((args, Some(rv)))
            }
            AnyFuncValue::Instance(instance) => self.instance_signature(instance),
        }
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
                    arg_tys.push(self.eval_paramless_for_returned_ty(
                        self.intern_fn_instance(arg.ty, Some(instance)),
                    )?);
                }
            }

            let ret_ty =
                match func.return_type {
                    Some(ty) => Some(self.eval_paramless_for_returned_ty(
                        self.intern_fn_instance(ty, Some(instance)),
                    )?),
                    None => None,
                };

            Ok((Obj::new_slice(&arg_tys, s), ret_ty))
        })
    }

    pub fn eval_generic_ensuring_conformance_no_reveal(
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

        let expected_ty = self.eval_paramless_for_returned_ty(
            self.intern_fn_instance(generic.ty, Some(instance_obj)),
        )?;

        let actual_value = match self.match_tys_with_coerce(actual_ty, expected_ty) {
            Ok(coercion) => self.apply_coercion_on_intern(coercion, actual_value),
            Err(_) => todo!(),
        };

        Ok(actual_value)
    }

    pub fn eval_generic_ensuring_conformance_with_reveal(
        &self,
        generic: Obj<Generic>,
        instance: Obj<FuncInstance>,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        self.eval_generic_ensuring_conformance_no_reveal(generic, instance)
            .map(|v| self.reveal_rich_value(v))
    }

    pub fn match_tys_with_coerce(
        &self,
        src: Obj<Ty>,
        target: Obj<Ty>,
    ) -> Result<Option<TyCoercion>, IncompatibleTypes> {
        let s = &self.session;

        if src == target {
            return Ok(None);
        }

        match (*src.r(s), *target.r(s)) {
            (Ty::FixedFunc(func), Ty::DynFunc(expected_args, expected_rv)) => {
                let Ok((actual_args, actual_rv)) = self.any_func_value_signature(func) else {
                    // The function could have any signature.
                    return Ok(None);
                };

                let actual_rv = actual_rv.unwrap();

                // Coercion and sub-typing are two different concepts. Coercion is an implicit
                // operation introduced during type-checking while sub-typing is the actual
                // operation used to determine type compatibility.
                //
                // In Rust, it is valid to coerce `fn() {my_func}` to `fn()` but it is not valid to
                // coerce `fn(fn() {my_func})` to `fn(fn())` because the function signatures are
                // checked using sub-typing rather than coercion...
                //
                // ```rust
                // fn main() {
                //     let f = helper(my_func);
                //     let mut g = helper(my_func as fn());
                //
                //     g = f;
                // }
                //
                // fn my_func() {}
                //
                // fn helper<T>(f: T) -> fn(T) {
                //     todo!()
                // }
                // ```
                //
                // ```
                // error[E0308]: mismatched types
                //  --> src/main.rs:5:9
                //   |
                // 5 |     g = f;
                //   |         ^ expected fn pointer, found fn item
                //   |
                //   = note: expected fn pointer `fn(fn())`
                //              found fn pointer `fn(fn() {my_func})`
                // ```
                //
                // Since we don't have sub-typing in this language (lol no regions), we just check
                // for exact type equality.
                if expected_args != actual_args {
                    return Err(IncompatibleTypes);
                }

                if expected_rv != actual_rv {
                    return Err(IncompatibleTypes);
                }

                Ok(Some(TyCoercion {
                    out_ty: target,
                    kind: TyCoercionKind::FixedFuncToDyn(func),
                }))
            }
            (Ty::FixedMetaTy(ty), Ty::DynMetaTy) => Ok(Some(TyCoercion {
                out_ty: target,
                kind: TyCoercionKind::FixedMetaTypeToDyn(ty),
            })),
            _ => Err(IncompatibleTypes),
        }
    }

    pub fn apply_coercion_on_intern(
        &self,
        coercion: Option<TyCoercion>,
        intern: ValuePlace,
    ) -> ValuePlace {
        let Some(coercion) = coercion else {
            return intern;
        };

        self.intern_from_scratch_arena(|dst_arena| {
            let dst_place = dst_arena.reserve(coercion.out_ty);

            self.apply_coercion(
                coercion.kind,
                Some(self.value_interner.arena()),
                intern,
                dst_arena,
                dst_place,
            );

            dst_place
        })
    }

    pub fn apply_coercion(
        &self,
        coercion: TyCoercionKind,
        src_arena: Option<&impl ValueArenaLike>,
        src_place: ValuePlace,
        dst_arena: &mut ValueArena,
        dst_place: ValuePlace,
    ) {
        _ = (src_arena, src_place);

        match coercion {
            TyCoercionKind::FixedFuncToDyn(fv) => {
                dst_arena.write_terminal(dst_place, ValueKind::DynFunc(Some(fv)));
            }
            TyCoercionKind::FixedMetaTypeToDyn(ty) => {
                dst_arena.write_terminal(dst_place, ValueKind::DynMetaType(Some(ty)));
            }
        }
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

    fn check_expr(&mut self, expr: Obj<Expr>, expected_ty: Option<Obj<Ty>>) -> Obj<Ty> {
        let s = &self.tcx.session;

        let actual_ty = self.check_expr_inner(expr);

        if let Some(expected_ty) = expected_ty {
            match self.tcx.match_tys_with_coerce(actual_ty, expected_ty) {
                Ok(Some(coercion)) => {
                    self.facts.coercions.insert(expr, coercion);
                }
                Ok(None) => {
                    // (empty)
                }
                Err(_) => {
                    todo!(
                        "type mismatch at {} in {expr:#?}: {expected_ty:#?}, {actual_ty:#?}",
                        expr.r(s).span
                    );
                }
            }
        }

        self.facts.expr_types_pre_coerce.insert(expr, actual_ty);

        expected_ty.unwrap_or(actual_ty)
    }

    fn check_expr_inner(&mut self, expr: Obj<Expr>) -> Obj<Ty> {
        let s = &self.tcx.session;

        match &expr.r(s).kind {
            ExprKind::Name(name) => match name {
                AnyName::Const(target) => {
                    let instance = self.tcx.intern_fn_instance(*target, Some(self.instance));

                    match self.tcx.eval_paramless_reveal_rich(instance) {
                        Ok(intern) => self.tcx.type_of_intern(intern),
                        Err(err) => self.err(err),
                    }
                }
                AnyName::Generic(ty) => {
                    match self
                        .tcx
                        .eval_generic_ensuring_conformance_with_reveal(*ty, self.instance)
                    {
                        Ok(value) => self.tcx.type_of_intern(value),
                        Err(err) => self.err(err),
                    }
                }
                AnyName::Local(local) => {
                    let Some(ty) = self.facts.local_types.get(local) else {
                        todo!();
                    };

                    *ty
                }
            },
            ExprKind::Block(block) => {
                for stmt in &block.r(s).stmts {
                    match stmt {
                        Stmt::Live(_) => {
                            // (nothing to do)
                        }
                        Stmt::Expr(expr) => {
                            self.check_expr(*expr, None);
                        }
                    }
                }

                if let Some(last_expr) = block.r(s).last_expr {
                    self.check_expr(last_expr, None)
                } else {
                    self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[])))
                }
            }
            ExprKind::Lit(kind) => match kind {
                LiteralKind::BoolLit(_) => self.tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)),
                LiteralKind::StrLit(_) => self.tcx.intern_ty(Ty::MetaString),
                LiteralKind::CharLit(token_char_lit) => todo!(),
                LiteralKind::NumLit(lit) => todo!(),
            },
            ExprKind::BinOp(bin_op_kind, obj, obj1) => todo!(),
            ExprKind::Call(callee, args) => {
                let callee = self.check_expr(*callee, None);

                let (expected_args, expected_rv, kind) = match callee.r(s) {
                    Ty::DynFunc(expected_args, expected_rv) => {
                        (*expected_args, *expected_rv, CallExprKind::Dynamic)
                    }
                    Ty::FixedFunc(func) => {
                        let (expected_args, expected_rv) =
                            match self.tcx.any_func_value_signature(*func) {
                                Ok(v) => v,
                                Err(err) => return self.err(err),
                            };

                        (
                            expected_args,
                            expected_rv.unwrap(),
                            CallExprKind::Fixed(*func),
                        )
                    }
                    _ => todo!(),
                };

                self.facts.call_exprs.insert(expr, kind);

                if args.len() != expected_args.r(s).len() {
                    todo!()
                }

                for (&arg, &expected_ty) in args.iter().zip(expected_args.r(s)) {
                    self.check_expr(arg, Some(expected_ty));
                }

                expected_rv
            }
            ExprKind::Destructure(pat, init) => {
                let init_ty = self.check_expr(*init, None);
                self.check_pat(*pat, init_ty);

                self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[])))
            }
            ExprKind::Assign(lhs, rhs) => {
                let muta = self.check_place_mutability(*lhs);

                if muta.is_not() {
                    todo!("not mutable");
                }

                let lhs_ty = self.check_expr(*lhs, None);
                self.check_expr(*rhs, Some(lhs_ty));

                self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&[])))
            }
            ExprKind::NamedIndex(lhs, name) => {
                let lhs_ty = self.check_expr(*lhs, None);

                let (index_kind, index_result) = match lhs_ty.r(s) {
                    Ty::FixedMetaTy(ty) => {
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

                        match self.tcx.eval_paramless_reveal_rich(member) {
                            Ok(value) => (
                                IndexExprKind::ConstMember(value),
                                self.tcx.type_of_intern(value),
                            ),
                            Err(err) => {
                                return self.err(err);
                            }
                        }
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

                self.tcx.intern_ty(Ty::FixedMetaTy(type_represented))
            }
            ExprKind::Func(func) => {
                if func.r(s).inner.generics.is_empty() {
                    let instance = self.tcx.intern_fn_instance(*func, Some(self.instance));

                    self.tcx.queue_wf(WfRequirement::TypeCheck(instance));

                    self.tcx
                        .intern_ty(Ty::FixedFunc(AnyFuncValue::Instance(instance)))
                } else {
                    self.tcx.intern_ty(Ty::MetaFunc)
                }
            }
            ExprKind::Instantiate(callee, args) => {
                self.check_expr(*callee, Some(self.tcx.intern_ty(Ty::MetaFunc)));

                for arg in args {
                    self.check_expr(*arg, None);
                }

                self.tcx.intern_ty(Ty::MetaAny)
            }
            ExprKind::Intrinsic(name) => {
                let Some(intrinsic) = self.tcx.resolve_intrinsic(*name) else {
                    todo!();
                };

                self.tcx.type_of_intern(intrinsic)
            }
            ExprKind::NewTuple(fields) => {
                let tys = fields
                    .iter()
                    .map(|expr| self.check_expr(*expr, None))
                    .collect::<Vec<_>>();

                self.tcx.intern_ty(Ty::Tuple(self.tcx.intern_tys(&tys)))
            }
            ExprKind::NewTupleType(args) => {
                for &arg in args {
                    self.check_expr(arg, Some(self.tcx.intern_ty(Ty::DynMetaTy)));
                }

                self.tcx.intern_ty(Ty::DynMetaTy)
            }
            ExprKind::Error(err) => self.err(*err),
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
    pub expr_types_pre_coerce: FxHashMap<Obj<Expr>, Obj<Ty>>,
    pub local_types: FxHashMap<Obj<Local>, Obj<Ty>>,
    pub index_exprs: FxHashMap<Obj<Expr>, IndexExprKind>,
    pub call_exprs: FxHashMap<Obj<Expr>, CallExprKind>,
    pub coercions: FxHashMap<Obj<Expr>, TyCoercion>,
    pub return_ty: Obj<Ty>,
}

impl TypeCheckFacts {
    pub fn expr_type_post_coerce(&self, expr: Obj<Expr>) -> Obj<Ty> {
        match self.coercions.get(&expr) {
            Some(coercion) => coercion.out_ty,
            None => self.expr_types_pre_coerce[&expr],
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum IndexExprKind {
    AdtField(u32),
    TupleField(u32),
    ConstMember(ValuePlace),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CallExprKind {
    Dynamic,
    Fixed(AnyFuncValue),
}

pub struct IncompatibleTypes;

#[derive(Debug, Copy, Clone)]
pub struct TyCoercion {
    pub out_ty: Obj<Ty>,
    pub kind: TyCoercionKind,
}

#[derive(Debug, Copy, Clone)]
pub enum TyCoercionKind {
    FixedFuncToDyn(AnyFuncValue),
    FixedMetaTypeToDyn(Obj<Ty>),
}
