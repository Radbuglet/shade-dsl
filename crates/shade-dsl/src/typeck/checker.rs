use arid::{Handle, W};

use crate::{
    base::{ErrorGuaranteed, analysis::Memo},
    typeck::syntax::{FuncInstanceHandle, ValueHandle},
};

pub struct TypeChecker {
    check_func: Memo<FuncInstanceHandle, Result<(), ErrorGuaranteed>>,
}

impl TypeChecker {
    pub fn check_func(
        &mut self,
        instance: FuncInstanceHandle,
        w: W,
    ) -> Result<(), ErrorGuaranteed> {
        self.check_func.compute(instance, |_| {
            let ast = instance.r(w).func;

            // TODO

            Ok(())
        })
    }

    pub fn eval_func(
        &mut self,
        func: FuncInstanceHandle,
        args: &[ValueHandle],
        w: W,
    ) -> Result<ValueHandle, ErrorGuaranteed> {
        todo!()
    }
}
