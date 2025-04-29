use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::Gcx,
    semantic::syntax::{
        BoundInstance, FuncExpr, FuncExprKind, FuncLocal, InferVar, Instance, Ty, TyKind, TyRuntime,
    },
};

use super::Analyzer;

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    pub locals: FxHashMap<FuncLocal<'gcx>, Ty<'gcx>>,
    pub exprs: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

struct Typeck<'gcx> {
    gcx: Gcx<'gcx>,
    instance: Instance<'gcx>,
    instance_bound: BoundInstance<'gcx>,
}

impl<'gcx> Typeck<'gcx> {
    fn new(gcx: Gcx<'gcx>, instance: Instance<'gcx>) -> Self {
        todo!()
    }

    fn populate_expr(&mut self, expr: FuncExpr<'gcx>) {
        let gcx = self.gcx;
        let instance = self.instance_bound;

        match expr.kind {
            FuncExprKind::Local(def) => {
                self.equate(expr.ty, def.ty.resolve_unbound(gcx, instance));
            }
            FuncExprKind::Call(receiver, args) => {
                let ret_iv = self.fresh_infer();
                let args_iv = gcx
                    .type_list_interner
                    .intern_iter(gcx, (0..args.len()).map(|_| self.fresh_infer()));

                self.equate(expr.ty, ret_iv);

                for (arg, iv) in args.iter().zip(args_iv.iter()) {
                    self.equate(*iv, arg.ty);
                }

                self.equate(
                    receiver.ty,
                    gcx.type_interner
                        .intern(gcx, TyKind::Runtime(TyRuntime::Func(args_iv, ret_iv))),
                );
            }
            FuncExprKind::Const(target) => {
                let target = target.resolve(gcx, instance);

                self.equate(expr.ty, target.func.ret_type.resolve_unbound(gcx, target));
            }
            FuncExprKind::Ascribe(ascribed, ty) => {
                self.equate(expr.ty, ty);
                self.equate(expr.ty, ascribed.ty);
            }
            FuncExprKind::ProvideGeneric(meta_func, _argument) => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaFunc));

                self.equate(
                    meta_func.ty,
                    gcx.type_interner.intern(gcx, TyKind::MetaFunc),
                );
            }
            FuncExprKind::Instantiate(meta_func) => {
                // (the return type has to be inferred)

                self.equate(
                    meta_func.ty,
                    gcx.type_interner.intern(gcx, TyKind::MetaFunc),
                );
            }
            FuncExprKind::FuncLiteral(_def) => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaFunc));
            }
            FuncExprKind::TypeOf(_ty) => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
            }
            FuncExprKind::TypeWithField { target, name, ty } => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                self.equate(target.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                // TODO: Name
                self.equate(ty.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
            }
            FuncExprKind::TypeWithMethod {
                target,
                name,
                producer,
            } => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                self.equate(target.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                // TODO: Name
                self.equate(producer.ty, gcx.type_interner.intern(gcx, TyKind::MetaFunc));
            }
            FuncExprKind::TypeWithStatic {
                target,
                name,
                producer,
            } => {
                self.equate(expr.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                self.equate(target.ty, gcx.type_interner.intern(gcx, TyKind::MetaType));
                // TODO: Name
                self.equate(producer.ty, gcx.type_interner.intern(gcx, TyKind::MetaFunc));
            }
        }
    }

    fn fresh_infer_var(&mut self) -> InferVar {
        todo!()
    }

    fn fresh_infer(&mut self) -> Ty<'gcx> {
        self.gcx
            .type_interner
            .intern(self.gcx, TyKind::Infer(self.fresh_infer_var()))
    }

    fn equate(&mut self, lhs: Ty<'gcx>, rhs: Ty<'gcx>) {
        todo!()
    }

    fn progress(&mut self, analyzer: &mut Analyzer) {}

    fn finish(&mut self) -> TypeckResults<'gcx> {
        todo!()
    }
}

pub fn type_check<'gcx>(
    analyzer: &mut Analyzer<'gcx>,
    instance: Instance<'gcx>,
) -> &'gcx TypeckResults<'gcx> {
    assert!(instance.fully_specified());

    let mut solver = Typeck::new(analyzer.gcx(), instance);
    solver.populate_expr(instance.func.main);
    solver.progress(analyzer);
    analyzer.gcx().alloc(solver.finish())
}
