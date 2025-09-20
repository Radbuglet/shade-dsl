use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::tcx::TyCtxt,
        syntax::{FuncInstance, ValueArena, ValuePtr},
    },
};

impl TyCtxt {
    pub fn eval_func(
        &self,
        instance: Obj<FuncInstance>,
        args: &[ValuePtr],
        arena: &mut ValueArena,
    ) -> Result<ValuePtr, ErrorGuaranteed> {
        let s = &self.session;

        self.type_check(instance)?;

        todo!()
    }

    pub fn bytecode(&self, instance: Obj<FuncInstance>) {
        todo!()
    }
}
