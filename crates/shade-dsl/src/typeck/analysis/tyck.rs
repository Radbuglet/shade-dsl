use arid::{Handle as _, W};
use index_vec::IndexVec;

use crate::{
    base::ErrorGuaranteed,
    typeck::{
        analysis::AnalysisCtxt,
        syntax::{AnyName, BlockHandle, ExprHandle, ExprKind, FuncInstanceHandle, TyHandle},
    },
};

impl AnalysisCtxt {
    pub fn check_func(
        &mut self,
        instance: FuncInstanceHandle,
        w: W,
    ) -> Result<(), ErrorGuaranteed> {
        self.check_func.compute(instance, |_| {
            let func = instance.r(w).func;

            Ok(())
        })
    }

    fn check_block(
        &mut self,
        instance: FuncInstanceHandle,
        block: BlockHandle,
        w: W,
    ) -> Result<TyHandle, ErrorGuaranteed> {
        for stmt in block.r(w).stmts.clone().iter() {
            match stmt.r(w).kind {
                ExprKind::Name(name) => match name {
                    AnyName::Member { init, .. } => {
                        let init = self
                            .resolve_ancestor_instance(instance, init, IndexVec::new(), w)
                            .as_weak();

                        self.eval_func(init, &[], w);

                        todo!();
                    }
                    AnyName::Const(const_def_handle) => todo!(),
                    AnyName::Generic(generic_def_handle) => todo!(),
                    AnyName::Local(local_def_handle) => todo!(),
                },
                ExprKind::Block(ref strong) => todo!(),
                ExprKind::Lit(literal_kind) => todo!(),
                ExprKind::BinOp(bin_op_kind, ref strong, ref strong1) => todo!(),
                ExprKind::Call(ref strong, ref strongs) => todo!(),
                ExprKind::Destructure(ref strong, ref strong1) => todo!(),
                ExprKind::Match(ref expr_match) => todo!(),
                ExprKind::Adt(ref strong) => todo!(),
                ExprKind::Func(ref strong) => todo!(),
                ExprKind::Error(_) => {
                    // (ignored)
                }
                ExprKind::Placeholder => unreachable!(),
            }
        }

        todo!()
    }

    fn check_expr(
        &mut self,
        instance: FuncInstanceHandle,
        expr: ExprHandle,
        w: W,
    ) -> Result<TyHandle, ErrorGuaranteed> {
        todo!()
    }
}
