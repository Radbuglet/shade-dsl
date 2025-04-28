use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::{Dp, Gcx},
    semantic::syntax::{FuncExpr, FuncLocal, Instance, Ty, TyAdtSignature, Value},
};

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
    well_formed_adts: Dp<TyAdtSignature<'gcx>, ()>,
}

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    locals: FxHashMap<FuncLocal<'gcx>, Ty<'gcx>>,
    exprs: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn type_check(&mut self, instance: Instance<'gcx>) -> &'gcx TypeckResults<'gcx> {
        self.checked_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            // Unbind the function's types.
            let mut results = TypeckResults::default();

            for &arg in instance.func.arguments {
                results.locals.insert(
                    arg,
                    arg.ty
                        .resolve_in(self.gcx, instance.as_bound(self.gcx))
                        .expect_unbound(),
                );
            }

            let return_ty = instance
                .func
                .ret_type
                .resolve_in(self.gcx, instance.as_bound(self.gcx))
                .expect_unbound();

            // TODO

            // Convert inference types to inference variables.
            // TODO

            // Build up inference constraints.
            // TODO

            // Repeatedly attempt to resolve types and make progress towards unification.
            // TODO

            todo!()
        })
    }

    pub fn evaluate(&mut self, instance: Instance<'gcx>) -> Value<'gcx> {
        self.resolved_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            let typeck_results = self.type_check(instance);

            todo!()
        })
    }
}
