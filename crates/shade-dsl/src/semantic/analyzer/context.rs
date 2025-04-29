use crate::{
    base::{Dp, Gcx},
    semantic::{
        analyzer::{TypeckResults, type_check},
        syntax::{Instance, Value},
    },
};

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn gcx(&self) -> Gcx<'gcx> {
        self.gcx
    }

    pub fn type_check(&mut self, instance: Instance<'gcx>) -> &'gcx TypeckResults<'gcx> {
        self.checked_instances
            .clone()
            .compute(instance, |_| type_check(self, instance))
    }

    pub fn evaluate(&mut self, instance: Instance<'gcx>) -> Value<'gcx> {
        self.resolved_instances.clone().compute(instance, |_| {
            assert!(instance.fully_specified());

            let typeck_results = self.type_check(instance);

            todo!()
        })
    }
}
