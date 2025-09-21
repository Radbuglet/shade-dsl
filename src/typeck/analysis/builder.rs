//! Logic for lowering from a type-checked HIR into bytecode.

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::LiteralKind,
    typeck::{
        analysis::tcx::TyCtxt,
        syntax::{
            BycBinOp, BycFunction, BycInstr, BycPopMode, Expr, ExprKind, FuncInstance, ValueScalar,
        },
    },
};

impl TyCtxt {
    pub fn build_bytecode(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<Obj<BycFunction>, ErrorGuaranteed> {
        let s = &self.session;

        self.queries.build_bytecode.compute(instance, |_| {
            self.type_check(instance)?;

            let func = &*instance.r(s).func.r(s).inner;

            todo!()
        })
    }
}

struct BycBuilderCtxt<'a> {
    tcx: &'a TyCtxt,
    instance: Obj<FuncInstance>,
    instructions: Vec<BycInstr>,
}

impl<'a> BycBuilderCtxt<'a> {
    /// Lowers an expression such that, once the sequence of instructions complete, a place pointing
    /// to the result of the expression. Returns whether the value needs to be freed by the
    /// consumer (i.e. whether it's a temporary or a place owned by a local).
    #[must_use]
    fn lower_place(&mut self, expr: &Expr) -> BycPopMode {
        let s = &self.tcx.session;

        match &expr.kind {
            ExprKind::Name(any_name) => todo!(),
            ExprKind::Block(obj) => todo!(),
            ExprKind::Lit(lit) => {
                self.instructions.extend([
                    BycInstr::Allocate,
                    BycInstr::AssignLitScalar(Box::new(match lit {
                        LiteralKind::BoolLit(v) => ValueScalar::Bool(*v),
                        LiteralKind::StrLit(token_str_lit) => todo!(),
                        LiteralKind::CharLit(token_char_lit) => todo!(),
                        LiteralKind::NumLit(token_num_lit) => todo!(),
                    })),
                ]);

                BycPopMode::PopAndFree
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                let rhs_mode = self.lower_place(rhs.r(s));
                let lhs_mode = self.lower_place(lhs.r(s));

                self.instructions.extend([BycInstr::BinOp(
                    BycBinOp::Scalar(*op),
                    rhs_mode,
                    lhs_mode,
                )]);

                BycPopMode::PopAndFree
            }
            ExprKind::Call(obj, objs) => todo!(),
            ExprKind::Destructure(obj, obj1) => todo!(),
            ExprKind::Match(expr_match) => todo!(),
            ExprKind::Adt(obj) => todo!(),
            ExprKind::Func(obj) => todo!(),
            ExprKind::Error(_) | ExprKind::Placeholder => unreachable!(),
        }
    }
}
