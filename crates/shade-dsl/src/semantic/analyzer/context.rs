use ctx2d_utils::hash::FxHashMap;

use crate::{
    base::{Dp, Gcx},
    semantic::syntax::{FuncExpr, Instance, Ty, Value},
};

pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
    checked_instances: Dp<Instance<'gcx>, &'gcx TypeckResults<'gcx>>,
    resolved_instances: Dp<Instance<'gcx>, Value<'gcx>>,
}

impl<'gcx> Analyzer<'gcx> {
    pub fn check(&mut self, instance: Instance<'gcx>) -> &'gcx TypeckResults {
        todo!()
    }

    pub fn resolve(&mut self, instance: Instance<'gcx>) -> Value<'gcx> {
        todo!()
    }
}

#[derive(Default)]
pub struct TypeckResults<'gcx> {
    types: FxHashMap<FuncExpr<'gcx>, Ty<'gcx>>,
}
