use arid::{Handle, Object, Strong, W};
use index_vec::IndexVec;

use crate::{
    base::analysis::Memo,
    typeck::syntax::{
        FuncHandle, FuncInstance, FuncInstanceHandle, OwnConstIdx, OwnGenericIdx, ValueHandle,
    },
};

mod eval;
mod tyck;

#[derive(Default)]
pub struct AnalysisCtxt {
    check_func: Memo<FuncInstanceHandle, ()>,
    eval_const: Memo<(FuncInstanceHandle, OwnConstIdx), Strong<ValueHandle>>,
}

impl AnalysisCtxt {
    fn resolve_ancestor_instance(
        &mut self,
        descendant: FuncInstanceHandle,
        ancestor_adt: FuncHandle,
        generics: IndexVec<OwnGenericIdx, Strong<ValueHandle>>,
        w: W,
    ) -> Strong<FuncInstanceHandle> {
        // TODO: intern
        FuncInstance {
            func: ancestor_adt,
            parent: ancestor_adt.r(w).parent.map(|parent| {
                self.get_instance_for_ancestor_ir(descendant, parent, w)
                    .as_strong(w)
            }),
            generics,
            consts: IndexVec::new(),
        }
        .spawn(w)
    }

    fn get_instance_for_ancestor_ir(
        &mut self,
        descendant: FuncInstanceHandle,
        ancestor: FuncHandle,
        w: W,
    ) -> FuncInstanceHandle {
        let mut descendant = Some(descendant);

        while let Some(curr) = descendant {
            if curr.r(w).func == ancestor {
                return curr;
            }

            descendant = curr.r(w).parent.as_ref().map(|v| v.as_weak());
        }

        unreachable!()
    }
}
