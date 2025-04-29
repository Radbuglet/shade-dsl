use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::Gcx,
    semantic::syntax::{BoundInstance, FuncExpr, FuncLocal, Instance, Ty},
};

use super::{Analyzer, resolve_type};

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    pub locals: FxHashMap<FuncLocal<'gcx>, Ty<'gcx>>,
    pub exprs: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

pub struct Typeck<'gcx> {
    pub gcx: Gcx<'gcx>,
    pub results: TypeckResults<'gcx>,
    pub instance: Instance<'gcx>,
    pub instance_bound: BoundInstance<'gcx>,
}

impl<'gcx> Typeck<'gcx> {
    pub fn new(gcx: Gcx<'gcx>, instance: Instance<'gcx>) -> Self {
        assert!(instance.fully_specified());

        Self {
            gcx,
            results: TypeckResults::default(),
            instance,
            instance_bound: instance.as_bound(gcx),
        }
    }

    pub fn check_fn(&mut self, analyzer: &mut Analyzer<'gcx>) {
        let gcx = self.gcx;
        let context = self.instance_bound;
        let func = self.instance.func;

        // Assign types to inputs
        for (&local, &ty) in func.arguments.iter().zip(func.argument_types.iter()) {
            self.results
                .locals
                .insert(local, resolve_type(gcx, context, ty));
        }

        // Determine the expected output type.
        let expected_out = resolve_type(gcx, context, func.ret_type);

        // Check the entrypoint.
        let actual_out = self.check_expr(analyzer);
        self.check_equality(expected_out, actual_out);
    }

    pub fn check_expr(&mut self, analyzer: &mut Analyzer<'gcx>) -> Ty<'gcx> {
        let gcx = self.gcx;
        let context = self.instance;

        todo!()
    }

    pub fn check_equality(&mut self, expected: Ty<'gcx>, actual: Ty<'gcx>) {
        todo!()
    }
}
