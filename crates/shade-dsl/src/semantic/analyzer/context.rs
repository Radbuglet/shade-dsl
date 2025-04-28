use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::{Dp, Gcx},
    semantic::syntax::{FuncExpr, Instance, Ty, TyKind, Value, ValueKind},
};

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
}

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    types: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn resolve_type(&mut self, instance: Instance<'gcx>, ty: Ty<'gcx>) -> Ty<'gcx> {
        let TyKind::Unresolved(unresolved) = **ty else {
            return ty;
        };

        assert!(instance.fully_specified());
        assert!(unresolved.fully_specified());

        let resolved = unresolved
            .resolve_in(self.gcx, instance.as_bound(self.gcx))
            .expect_unbound(self.gcx);

        let ValueKind::MetaType(resolved) = **self.resolve(resolved) else {
            unreachable!();
        };

        resolved
    }

    pub fn check(&mut self, instance: Instance<'gcx>) -> &'gcx TypeckResults<'gcx> {
        self.checked_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            // Check the signature.
            todo!()
        })
    }

    pub fn resolve(&mut self, instance: Instance<'gcx>) -> Value<'gcx> {
        self.resolved_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            let typeck_results = self.check(instance);

            todo!()
        })
    }
}
