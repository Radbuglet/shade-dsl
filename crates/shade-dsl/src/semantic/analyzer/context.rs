use crate::{
    base::{Dp, Gcx},
    semantic::{
        analyzer::{Typeck, TypeckResults},
        syntax::{Instance, Ty, TyKind, Value, ValueKind},
    },
};

use super::rebind_type;

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn gcx(&self) -> Gcx<'gcx> {
        self.gcx
    }

    pub fn evaluate_type(&mut self, context: Instance<'gcx>, ty: Ty<'gcx>) -> Ty<'gcx> {
        assert!(context.fully_specified());

        let gcx = self.gcx;
        let ty = rebind_type(self.gcx, context.as_bound(gcx), ty);

        match **ty {
            TyKind::MetaType | TyKind::MetaFunc | TyKind::Scalar(_) | TyKind::Adt(_) => ty,
            TyKind::Const(instance) => {
                let instance = instance.expect_unbound(self.gcx);

                let value = self.evaluate_const(instance);
                let ValueKind::MetaType(ty) = value.kind else {
                    unreachable!();
                };

                ty
            }
            TyKind::Func(args, retval) => {
                let args =
                    gcx.intern_iter(args.iter().map(|&arg| self.evaluate_type(context, arg)));

                let retval = self.evaluate_type(context, retval);

                gcx.intern(TyKind::Func(args, retval))
            }
            TyKind::Tuple(elems) => gcx.intern(TyKind::Tuple(
                gcx.intern_iter(elems.iter().map(|&elem| self.evaluate_type(context, elem))),
            )),
            TyKind::Generic(_) => unreachable!(),
        }
    }

    pub fn type_check(&mut self, instance: Instance<'gcx>) -> &'gcx TypeckResults<'gcx> {
        self.checked_instances.clone().compute(instance, |_| {
            let mut tcx = Typeck::new(self.gcx, instance);
            tcx.check_fn(self);

            self.gcx.alloc(tcx.results)
        })
    }

    pub fn evaluate_const(&mut self, instance: Instance<'gcx>) -> Value<'gcx> {
        self.resolved_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            let typeck_results = self.type_check(instance);

            todo!()
        })
    }
}
