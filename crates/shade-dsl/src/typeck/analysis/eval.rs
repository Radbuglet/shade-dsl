use arid::{Strong, W};

use crate::{
    base::ErrorGuaranteed,
    typeck::{
        analysis::AnalysisCtxt,
        syntax::{ConstDefHandle, FuncInstanceHandle, ValueHandle},
    },
};

impl AnalysisCtxt {
    pub fn eval_const(
        &mut self,
        instance: FuncInstanceHandle,
        cst_def: ConstDefHandle,
        w: W,
    ) -> Result<Strong<ValueHandle>, ErrorGuaranteed> {
        todo!();
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
