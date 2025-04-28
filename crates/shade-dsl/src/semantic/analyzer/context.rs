use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::{Dp, Gcx},
    semantic::syntax::{
        FuncExpr, FuncLocal, Instance, Ty, UnresolvedTy, UnresolvedTyKind, Value, ValueKind,
    },
};

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
}

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    locals: FxHashMap<FuncLocal<'gcx>, Ty<'gcx>>,
    exprs: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn resolve_type(&mut self, instance: Instance<'gcx>, ty: UnresolvedTy<'gcx>) -> Ty<'gcx> {
        let sub_instance = match **ty {
            UnresolvedTyKind::Resolved(ty) => return ty,
            UnresolvedTyKind::Unresolved(sub_instance) => sub_instance,
        };

        assert!(instance.fully_specified());
        assert!(sub_instance.fully_specified());

        let resolved = sub_instance
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

            // Resolve the signature's types.
            let mut results = TypeckResults::default();

            for &arg in instance.func.arguments {
                results
                    .locals
                    .insert(arg, self.resolve_type(instance, arg.ty));
            }

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
