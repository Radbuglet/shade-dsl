use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::Gcx,
    semantic::syntax::{FuncExpr, FuncExprKind, FuncLocal, Instance, Ty, TyKind},
};

use super::Analyzer;

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    pub locals: FxHashMap<FuncLocal<'gcx>, Ty<'gcx>>,
    pub exprs: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

pub struct Typeck<'gcx> {
    pub gcx: Gcx<'gcx>,
    pub results: TypeckResults<'gcx>,
    pub instance: Instance<'gcx>,
}

impl<'gcx> Typeck<'gcx> {
    pub fn new(gcx: Gcx<'gcx>, instance: Instance<'gcx>) -> Self {
        assert!(instance.fully_specified());

        Self {
            gcx,
            results: TypeckResults::default(),
            instance,
        }
    }

    pub fn check_fn(&mut self, analyzer: &mut Analyzer<'gcx>) {
        let context = self.instance;
        let func = context.func;

        // Assign types to inputs
        for (&local, &ty) in func.arguments.iter().zip(func.argument_types.iter()) {
            self.results
                .locals
                .insert(local, analyzer.evaluate_type(context, ty));
        }

        // Determine the expected output type.
        let expected_out = analyzer.evaluate_type(context, func.ret_type);

        // Check the entrypoint.
        let actual_out = self.check_expr(analyzer, func.main);
        self.check_compat(expected_out, actual_out);
    }

    pub fn check_expr(&mut self, analyzer: &mut Analyzer<'gcx>, expr: FuncExpr<'gcx>) -> Ty<'gcx> {
        let ty = match **expr {
            FuncExprKind::Local(local) => self.results.locals[&local],
            FuncExprKind::Call(callee, args) => {
                let callee = self.check_expr(analyzer, callee);

                let (expected_args, expected_ret_val) = match **callee {
                    TyKind::MetaType
                    | TyKind::Adt(_)
                    | TyKind::Tuple(_)
                    | TyKind::Scalar(_)
                    | TyKind::MetaFunc => {
                        panic!("unsupported type");
                    }
                    TyKind::Func(expected_args, expected_ret_val) => {
                        (expected_args, expected_ret_val)
                    }
                    TyKind::Const(_) | TyKind::Generic(_) => unreachable!(),
                };

                assert_eq!(args.len(), expected_args.len());

                for (&arg, &expected_ty) in args.iter().zip(expected_args.iter()) {
                    let actual_ty = self.check_expr(analyzer, arg);
                    self.check_compat(expected_ty, actual_ty);
                }

                expected_ret_val
            }
            FuncExprKind::Const(cst) => {
                analyzer
                    .evaluate_const(cst.rebind_unbound(self.gcx, self.instance.as_bound(self.gcx)))
                    // We could alternatively get this from the signature but doing it this way
                    // ensures that we also type-check all embedded constants.
                    .ty
            }
            FuncExprKind::ProvideGeneric(meta_fn, meta_arg) => {
                let meta_fn_expected = self.gcx.interned.meta_fn;
                let meta_fn_actual = self.check_expr(analyzer, meta_fn);

                self.check_compat(meta_fn_expected, meta_fn_actual);

                // `meta_arg` can be anything we want—it will be checked during evaluation.
                self.check_expr(analyzer, meta_arg);

                meta_fn_expected
            }
            FuncExprKind::Instantiate(meta_fn, expected_ty) => {
                let meta_fn_expected = self.gcx.interned.meta_fn;
                let meta_fn_actual = self.check_expr(analyzer, meta_fn);

                self.check_compat(meta_fn_expected, meta_fn_actual);

                analyzer.evaluate_type(self.instance, expected_ty)
            }
            FuncExprKind::FuncLiteral(_) => self.gcx.interned.meta_fn,
            FuncExprKind::TypeOf(ty) => {
                let _ = analyzer.evaluate_type(self.instance, ty);

                self.gcx.interned.meta_ty
            }
            FuncExprKind::TypeWithField { target, name, ty } => {
                let target_expected = self.gcx.interned.meta_ty;
                let target_actual = self.check_expr(analyzer, target);
                self.check_compat(target_expected, target_actual);

                // TODO: name

                let ty_expected = self.gcx.interned.meta_ty;
                let ty_actual = self.check_expr(analyzer, ty);
                self.check_compat(ty_expected, ty_actual);

                self.gcx.interned.meta_ty
            }
            FuncExprKind::TypeWithMethod {
                target,
                name,
                producer,
            } => {
                let target_expected = self.gcx.interned.meta_ty;
                let target_actual = self.check_expr(analyzer, target);
                self.check_compat(target_expected, target_actual);

                // TODO: name

                let producer_expected = self.gcx.interned.meta_producer_fn;
                let producer_actual = self.check_expr(analyzer, producer);
                self.check_compat(producer_expected, producer_actual);

                self.gcx.interned.meta_ty
            }
            FuncExprKind::TypeWithStatic {
                target,
                name,
                producer,
            } => {
                let target_expected = self.gcx.interned.meta_ty;
                let target_actual = self.check_expr(analyzer, target);
                self.check_compat(target_expected, target_actual);

                // TODO: name

                let producer_expected = self.gcx.interned.meta_producer_fn;
                let producer_actual = self.check_expr(analyzer, producer);
                self.check_compat(producer_expected, producer_actual);

                self.gcx.interned.meta_ty
            }
        };

        self.results.exprs.insert(expr, ty);

        ty
    }

    pub fn check_compat(&mut self, expected: Ty<'gcx>, actual: Ty<'gcx>) {
        assert!(expected == actual);
    }
}
