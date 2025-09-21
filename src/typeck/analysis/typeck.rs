use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{analysis::tcx::TyCtxt, syntax::FuncInstance},
};

impl TyCtxt {
    pub fn type_check(&self, instance: Obj<FuncInstance>) -> Result<(), ErrorGuaranteed> {
        let s = &self.session;

        self.queries.type_check.compute(instance, |_| {
            let func = &*instance.r(s).func.r(s).inner;

            // TODO

            Ok(())
        })
    }
}
