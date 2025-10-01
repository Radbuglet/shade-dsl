//! Logic for lowering from a type-checked HIR into bytecode.

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::LiteralKind,
    typeck::{
        analysis::{TypeCheckFacts, tcx::TyCtxt},
        syntax::{
            AdtInstance, AnyFuncValue, AnyMetaFuncValue, AnyName, BycBinOp, BycFunction, BycInstr,
            BycPopMode, Expr, ExprKind, FuncInstance, MetaFuncInstance, ScalarKind, Ty, Value,
            ValueKind, ValueScalar,
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
            let facts = self.type_check(instance)?;
            let func = &*instance.r(s).func.r(s).inner;

            let mut ctxt = BycBuilderCtxt {
                tcx: self,
                instance,
                facts: facts.r(s),
                instructions: Vec::new(),
            };

            let mut depth = 0;
            ctxt.lower_expr_for_value(func.body.r(s), &mut depth);
            ctxt.push([BycInstr::Return], &mut depth);

            Ok(ctxt.finish())
        })
    }
}

struct BycBuilderCtxt<'a> {
    tcx: &'a TyCtxt,
    instance: Obj<FuncInstance>,
    facts: &'a TypeCheckFacts,
    instructions: Vec<BycInstr>,
}

impl<'a> BycBuilderCtxt<'a> {
    fn push(&mut self, instructions: impl IntoIterator<Item = BycInstr>, depth: &mut u32) {
        self.instructions
            .extend(instructions.into_iter().inspect(|instr| {
                *depth = depth.checked_add_signed(instr.depth_delta()).unwrap();
            }));
    }

    fn lower_expr_for_value(&mut self, expr: &Expr, depth: &mut u32) {
        let mode = self.lower_expr_maybe_place(expr, depth);

        if !mode.needs_free() {
            self.push([BycInstr::ShallowCopy], depth);
        }
    }

    /// Lowers an expression such that, once the sequence of instructions complete, a place pointing
    /// to the result of the expression. Returns whether the value needs to be freed by the
    /// consumer (i.e. whether it's a temporary or a place owned by a local).
    #[must_use]
    fn lower_expr_maybe_place(&mut self, expr: &Expr, depth: &mut u32) -> BycPopMode {
        let old_depth = *depth;
        let mode = self.lower_expr_maybe_place_inner(expr, depth);
        debug_assert_eq!(
            *depth,
            old_depth + 1,
            "the stack was broken by...\n{expr:#?}\nexpected just one place to be pushed to the stack but got {}",
            (*depth as i32) - (old_depth as i32),
        );
        mode
    }

    fn lower_expr_maybe_place_inner(&mut self, expr: &Expr, depth: &mut u32) -> BycPopMode {
        let s = &self.tcx.session;

        match &expr.kind {
            ExprKind::Name(name) => match name {
                AnyName::Const(cst) => {
                    let cst = self.tcx.intern_fn_instance(*cst, Some(self.instance));
                    self.push([BycInstr::ConstEval(cst)], depth);

                    BycPopMode::PopAndFree
                }
                AnyName::Generic(generic) => {
                    let cst = self
                        .tcx
                        .eval_generic_ensuring_conformance(*generic, self.instance)
                        .unwrap();

                    self.push([BycInstr::AllocConst(cst)], depth);

                    BycPopMode::PopAndFree
                }
                AnyName::Local(obj) => todo!(),
            },
            ExprKind::Block(obj) => todo!(),
            ExprKind::Lit(lit) => {
                self.push(
                    [BycInstr::AllocConst(self.tcx.intern_from_scratch_arena(
                        |arena| match lit {
                            LiteralKind::BoolLit(v) => arena.alloc(Value {
                                ty: self.tcx.intern_ty(Ty::Scalar(ScalarKind::Bool)),
                                kind: ValueKind::Scalar(ValueScalar::Bool(*v)),
                            }),
                            LiteralKind::StrLit(str) => arena.alloc(Value {
                                ty: self.tcx.intern_ty(Ty::MetaString),
                                kind: ValueKind::MetaString(str.value),
                            }),
                            LiteralKind::CharLit(token_char_lit) => todo!(),
                            LiteralKind::NumLit(token_num_lit) => todo!(),
                        },
                    ))],
                    depth,
                );

                BycPopMode::PopAndFree
            }
            ExprKind::Intrinsic(id) => {
                let intern = self.tcx.resolve_intrinsic(*id).unwrap();

                self.push([BycInstr::AllocConst(intern)], depth);

                BycPopMode::PopAndFree
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                let rhs_mode = self.lower_expr_maybe_place(rhs.r(s), depth);
                let lhs_mode = self.lower_expr_maybe_place(lhs.r(s), depth);

                self.push(
                    [BycInstr::BinOp(BycBinOp::Scalar(*op), rhs_mode, lhs_mode)],
                    depth,
                );

                BycPopMode::PopAndFree
            }
            ExprKind::Call(callee, args) => {
                self.lower_expr_for_value(callee.r(s), depth);

                for &arg in args {
                    self.lower_expr_for_value(arg.r(s), depth);
                }

                self.push(
                    [
                        BycInstr::CallStart(args.len() as u32),
                        BycInstr::CallCleanup(args.len() as u32),
                    ],
                    depth,
                );

                BycPopMode::PopAndFree
            }
            ExprKind::Destructure(obj, obj1) => todo!(),
            ExprKind::Match(expr_match) => todo!(),
            ExprKind::Adt(adt) => {
                self.push(
                    [BycInstr::AllocType(self.tcx.intern_ty(Ty::Adt(
                        AdtInstance {
                            owner: self.instance,
                            adt: *adt,
                        },
                    )))],
                    depth,
                );

                BycPopMode::PopAndFree
            }
            ExprKind::NewTuple(fields) => {
                for &field in fields {
                    self.lower_expr_for_value(field.r(s), depth);
                }

                self.push([BycInstr::NewTuple(fields.len() as u32)], depth);

                BycPopMode::PopAndFree
            }
            ExprKind::NewTupleType(fields) => {
                for &field in fields {
                    self.lower_expr_for_value(field.r(s), depth);
                }

                self.push([BycInstr::NewTupleType(fields.len() as u32)], depth);

                BycPopMode::PopAndFree
            }
            ExprKind::Func(func) => {
                let value = if func.r(s).inner.generics.is_empty() {
                    let instance = self.tcx.intern_fn_instance(*func, Some(self.instance));
                    Value {
                        ty: self.tcx.instance_fn_ty(instance).unwrap(),
                        kind: ValueKind::Func(AnyFuncValue::Instance(instance)),
                    }
                } else {
                    Value {
                        ty: self.tcx.intern_ty(Ty::MetaFunc),
                        kind: ValueKind::MetaFunc(AnyMetaFuncValue::Instance(MetaFuncInstance {
                            func: *func,
                            parent: Some(self.instance),
                        })),
                    }
                };

                let value = self
                    .tcx
                    .intern_from_scratch_arena(|arena| arena.alloc(value));

                self.push([BycInstr::AllocConst(value)], depth);

                BycPopMode::PopAndFree
            }
            ExprKind::Instantiate(target, generics) => {
                self.lower_expr_for_value(target.r(s), depth);

                for generic in generics {
                    self.lower_expr_for_value(generic.r(s), depth);
                }

                self.push([BycInstr::Instantiate(generics.len() as u32)], depth);

                BycPopMode::PopAndFree
            }
            ExprKind::Error(_) => unreachable!(),
        }
    }

    fn finish(self) -> Obj<BycFunction> {
        Obj::new(
            BycFunction {
                instance: self.instance,
                instructions: self.instructions,
            },
            &self.tcx.session,
        )
    }
}
