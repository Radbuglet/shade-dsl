use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::tcx::TyCtxt,
        syntax::{FuncInstance, Generic, Ty, TyList, ValuePlace},
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

            // Determine the function's signature.
            let (expected_args, expected_ret) = self.instance_signature(instance)?;

            // Type-check the body with all expectations.
            // TODO

            Ok(())
        })
    }

    pub fn instance_fn_ty(&self, instance: Obj<FuncInstance>) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let (args, ret_ty) = self.instance_signature(instance)?;
        Ok(self.intern_ty(Ty::Func(args, ret_ty.unwrap())))
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
                    arg_tys.push(self.eval_ty(self.intern_fn_instance(arg.ty, Some(instance)))?);
                }
            }

            let ret_ty = match func.return_type {
                Some(ty) => Some(self.eval_ty(self.intern_fn_instance(ty, Some(instance)))?),
                None => None,
            };

            Ok((Obj::new_slice(&arg_tys, s), ret_ty))
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
