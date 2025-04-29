use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::Gcx,
    semantic::syntax::{
        BoundInstance, BoundTy, FuncExpr, FuncExprInner, FuncExprKind, FuncLocal, Instance, Ty,
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
        todo!()
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
