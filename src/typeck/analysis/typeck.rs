use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::tcx::TyCtxt,
        syntax::{FuncInstance, Generic, ValuePlace},
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

            // Determine the types of the arguments.
            // TODO

            // Type-check the body with all expectations.
            // TODO

            Ok(())
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

        let expected_ty = self.eval_ty(self.intern_fn_instance(generic.ty, Some(instance_obj)))?;

        if expected_ty != actual_ty {
            todo!();
        }

        Ok(actual_value)
    }
}
