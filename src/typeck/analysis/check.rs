use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::tcx::TyCtxt,
        syntax::{Func, FuncInstance, Ty, ValueArena, ValueKind, ValuePtr},
    },
};

impl TyCtxt {
    pub fn eval_paramless(&self, instance: Obj<FuncInstance>) -> Result<ValuePtr, ErrorGuaranteed> {
        self.queries.eval_paramless.compute(instance, |_| {
            let mut arena = ValueArena::default();
            let root = self.eval_func(instance, &[], &mut arena)?;
            Ok(self.value_interner.intern(&arena, root))
        })
    }

    pub fn eval_const(
        &self,
        ty: Obj<Func>,
        parent: Obj<FuncInstance>,
    ) -> Result<ValuePtr, ErrorGuaranteed> {
        self.eval_paramless(self.intern_fn_instance(ty, Some(parent)))
    }

    pub fn eval_ty(
        &self,
        ty: Obj<Func>,
        parent: Obj<FuncInstance>,
    ) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let value = self.eval_paramless(self.intern_fn_instance(ty, Some(parent)))?;

        let ValueKind::MetaType(ty) = self.value_interner.read(value).kind else {
            todo!();
        };

        Ok(ty)
    }

    pub fn ty_from_value(&self, value: ValuePtr) -> Result<Obj<Ty>, ErrorGuaranteed> {
        todo!()
    }

    pub fn match_ty(&self, actual: Obj<Ty>, expected: Obj<Ty>) -> Result<(), ErrorGuaranteed> {
        if actual != expected {
            todo!()
        }

        Ok(())
    }

    pub fn type_check(&self, instance: Obj<FuncInstance>) -> Result<(), ErrorGuaranteed> {
        let s = &self.session;

        self.queries.type_check.compute(instance, |_| {
            let func = &*instance.r(s).func.r(s).inner;

            // TODO

            Ok(())
        })
    }
}
