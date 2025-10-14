//! Logic for lowering from a type-checked HIR into bytecode.

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::LiteralKind,
    typeck::{
        analysis::{TypeCheckFacts, tcx::TyCtxt},
        syntax::{
            AdtInstance, AnyFuncValue, AnyMetaFuncValue, AnyName, BycDepth, BycFunction, BycInstr,
            Expr, ExprKind, FuncInstance, Local, MetaFuncInstance, Pat, PatKind, Stmt, Ty,
            ValueKind, ValueScalar, byc_instr, visit_named_places,
        },
    },
    utils::hash::FxHashMap,
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

            let (fn_arg_tys, _fn_ret_ty) = self.instance_signature(instance).unwrap();

            let mut ctxt = BycBuilderCtxt {
                tcx: self,
                instance,
                facts: facts.r(s),
                locals: FxHashMap::default(),
                instructions: Vec::new(),
                depth: BycDepth::ZERO,
            };

            ctxt.scope_locals(|ctxt| {
                let params = func.params.as_ref().map_or(&[][..], |v| v.as_slice());

                for (arg_idx, arg) in params.iter().enumerate() {
                    cbit::cbit!(
                        for local in visit_named_places(arg.binding, &self.session) {
                            ctxt.push(byc_instr::Allocate {
                                ty: fn_arg_tys.r(s)[arg_idx],
                            });
                            ctxt.locals.insert(local, ctxt.depth.local as u32);
                        }
                    );

                    ctxt.push(byc_instr::Tee { at: arg_idx as u32 });
                    ctxt.lower_pat_assigns(arg.binding.r(s));
                }

                ctxt.push(byc_instr::Tee {
                    at: params.len() as u32 + 1,
                });
                ctxt.lower_expr_for_operand(func.body);
                ctxt.push(byc_instr::Forget { count: 1 });
                ctxt.push(byc_instr::Return {});
            });

            debug_assert_eq!(ctxt.depth, BycDepth::ZERO);

            Ok(ctxt.finish())
        })
    }
}

struct BycBuilderCtxt<'a> {
    tcx: &'a TyCtxt,
    instance: Obj<FuncInstance>,
    facts: &'a TypeCheckFacts,
    locals: FxHashMap<Obj<Local>, u32>,
    instructions: Vec<BycInstr>,
    depth: BycDepth,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ExprLowerMode {
    /// Lower the expression to a place pushed to the top of the **place stack**.
    Place,

    /// Lower the expression as an assignment to the extant place on the top of the **place stack**.
    Operand,
}

impl<'a> BycBuilderCtxt<'a> {
    fn push(&mut self, instr: impl Into<BycInstr>) {
        let instr = instr.into();
        self.depth += instr.depth_delta();
        self.instructions.push(instr);
    }

    fn scope_locals(&mut self, f: impl FnOnce(&mut Self)) {
        let orig_locals = self.depth.local;
        f(self);

        if let Some(count) = (self.depth.local as u32).checked_sub(orig_locals as u32)
            && count != 0
        {
            self.push(byc_instr::Deallocate { count });
        }
    }

    fn adapt_place(&mut self, expected_mode: ExprLowerMode, f: impl FnOnce(&mut Self)) {
        match expected_mode {
            ExprLowerMode::Place => {
                f(self);
            }
            ExprLowerMode::Operand => {
                f(self);
                self.push(byc_instr::AssignCopyRhsThenLhs {});
            }
        }
    }

    fn adapt_operand(
        &mut self,
        expr_ty: Obj<Ty>,
        expected_mode: ExprLowerMode,
        f: impl FnOnce(&mut Self),
    ) {
        match expected_mode {
            ExprLowerMode::Place => {
                self.push(byc_instr::Allocate { ty: expr_ty });
                self.push(byc_instr::Reference { at: 0 });
                f(self);
            }
            ExprLowerMode::Operand => {
                f(self);
            }
        }
    }

    fn lower_expr_for_direct(&mut self, expr: Obj<Expr>) {
        let old_depth = self.depth.place;
        self.lower_expr_inner(ExprLowerMode::Place, expr);
        debug_assert_eq!(
            self.depth.place,
            old_depth + 1,
            "stack broken while lowering for operand (dt = 1)...\n{expr:#?}"
        );
    }

    fn lower_expr_for_operand(&mut self, expr: Obj<Expr>) {
        let old_depth = self.depth.place;
        self.lower_expr_inner(ExprLowerMode::Operand, expr);
        debug_assert_eq!(
            self.depth.place, old_depth,
            "stack broken while lowering for operand (dt = 0)...\n{expr:#?}"
        );
    }

    fn lower_expr_for_copy(&mut self, expr: Obj<Expr>) {
        self.push(byc_instr::Allocate {
            ty: self.facts.expr_types[&expr],
        });
        self.push(byc_instr::Reference { at: 0 });
        self.lower_expr_for_operand(expr);
    }

    fn lower_expr_inner(&mut self, expected_mode: ExprLowerMode, expr: Obj<Expr>) {
        let s = &self.tcx.session;

        let expr_ty = self.facts.expr_types[&expr];

        match &expr.r(s).kind {
            ExprKind::Name(name) => match name {
                AnyName::Const(cst) => {
                    let cst = self.tcx.intern_fn_instance(*cst, Some(self.instance));

                    self.adapt_operand(expr_ty, expected_mode, |this| {
                        this.push(byc_instr::AssignConstExpr { func: cst });
                    });
                }
                AnyName::Generic(generic) => {
                    let cst = self
                        .tcx
                        .eval_generic_ensuring_conformance(*generic, self.instance)
                        .unwrap();

                    self.adapt_operand(expr_ty, expected_mode, |this| {
                        this.push(byc_instr::AssignConst { intern: cst });
                    });
                }
                AnyName::Local(local) => {
                    self.adapt_place(expected_mode, |this| {
                        this.push(byc_instr::Reference {
                            at: this.depth.local as u32 - this.locals[local],
                        });
                    });
                }
            },
            ExprKind::Block(block) => {
                self.scope_locals(|this| {
                    for stmt in &block.r(s).stmts {
                        match stmt {
                            Stmt::Live(local) => {
                                this.push(byc_instr::Allocate { ty: expr_ty });
                                this.locals.insert(*local, this.depth.local as u32);
                            }
                            Stmt::Expr(expr) => {
                                this.scope_locals(|this| {
                                    this.lower_expr_for_direct(*expr);
                                    this.push(byc_instr::Forget { count: 1 });
                                });
                            }
                        }
                    }

                    if let Some(last_expr) = block.r(s).last_expr {
                        match expected_mode {
                            ExprLowerMode::Place => this.lower_expr_for_direct(last_expr),
                            ExprLowerMode::Operand => this.lower_expr_for_operand(last_expr),
                        }
                    } else {
                        this.adapt_operand(expr_ty, expected_mode, |_this| {
                            // (tuples are initialized at reservation time)
                        });
                    }
                });
            }
            ExprKind::Lit(lit) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.push(byc_instr::AssignConst {
                        intern: self.tcx.intern_from_scratch_arena(|arena| match lit {
                            LiteralKind::BoolLit(v) => arena.alloc_terminal(
                                expr_ty,
                                ValueKind::Scalar(Some(ValueScalar::Bool(*v))),
                            ),
                            LiteralKind::StrLit(str) => arena
                                .alloc_terminal(expr_ty, ValueKind::MetaString(Some(str.value))),
                            LiteralKind::CharLit(token_char_lit) => todo!(),
                            LiteralKind::NumLit(token_num_lit) => todo!(),
                        }),
                    });
                });
            }
            ExprKind::Intrinsic(id) => {
                let intern = self.tcx.resolve_intrinsic(*id).unwrap();

                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.push(byc_instr::AssignConst { intern });
                });
            }
            ExprKind::BinOp(op, lhs, rhs) => {
                self.lower_expr_for_direct(*rhs);
                self.lower_expr_for_direct(*lhs);

                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.push(byc_instr::BinOp { op: *op });
                });
            }
            ExprKind::Call(callee, args) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.lower_expr_for_direct(*callee);

                    for &arg in args {
                        this.lower_expr_for_copy(arg);
                    }

                    this.push(byc_instr::Call {
                        args: args.len() as u32,
                    });
                    this.push(byc_instr::Forget {
                        count: args.len() as u32 + 1,
                    });
                });
            }
            ExprKind::Destructure(pat, rhs) => {
                self.lower_expr_for_direct(*rhs);
                self.lower_pat_assigns(pat.r(s));

                self.adapt_operand(expr_ty, expected_mode, |_this| {
                    // (tuples are initialized at reservation time)
                });
            }
            ExprKind::Assign(lhs, rhs) => {
                self.lower_expr_for_direct(*lhs);
                self.lower_expr_for_operand(*rhs);
                self.push(byc_instr::Forget { count: 1 });

                self.adapt_operand(expr_ty, expected_mode, |_this| {
                    // (tuples are initialized at reservation time)
                });
            }
            ExprKind::NamedIndex(lhs, name) => {
                todo!();
            }
            ExprKind::Match(expr_match) => todo!(),
            ExprKind::Adt(adt) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.push(byc_instr::AssignTypeLiteral {
                        ty: this.tcx.intern_ty(Ty::Adt(AdtInstance {
                            owner: this.instance,
                            adt: *adt,
                        })),
                    });
                });
            }
            ExprKind::NewTuple(fields) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    for (idx, field) in fields.iter().enumerate() {
                        this.push(byc_instr::Tee { at: 0 });
                        this.push(byc_instr::TupleIndex { idx: idx as u32 });
                        this.lower_expr_for_operand(*field);
                        this.push(byc_instr::Forget { count: 1 });
                    }
                });
            }
            ExprKind::NewTupleType(fields) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    for &field in fields {
                        this.lower_expr_for_direct(field);
                    }

                    this.push(byc_instr::AssignTupleType {
                        fields: fields.len() as u32,
                    });
                });
            }
            ExprKind::Func(func) => {
                let value = if func.r(s).inner.generics.is_empty() {
                    let instance = self.tcx.intern_fn_instance(*func, Some(self.instance));

                    ValueKind::Func(Some(AnyFuncValue::Instance(instance)))
                } else {
                    ValueKind::MetaFunc(Some(AnyMetaFuncValue::Instance(MetaFuncInstance {
                        func: *func,
                        parent: Some(self.instance),
                    })))
                };

                let value = self
                    .tcx
                    .intern_from_scratch_arena(|arena| arena.alloc_terminal(expr_ty, value));

                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.push(byc_instr::AssignConst { intern: value });
                });
            }
            ExprKind::Instantiate(target, generics) => {
                self.adapt_operand(expr_ty, expected_mode, |this| {
                    this.lower_expr_for_direct(*target);

                    for &generic in generics {
                        this.lower_expr_for_direct(generic);
                    }

                    this.push(byc_instr::Instantiate {
                        args: generics.len() as u32,
                    });
                });
            }
            ExprKind::Error(_) => unreachable!(),
        }
    }

    // Input: the top of the stack points to the place to which we're writing and it must be popped
    //  off by the time the emitted code finishes.
    fn lower_pat_assigns(&mut self, pat: &Pat) {
        match &pat.kind {
            PatKind::Hole => {
                self.push(byc_instr::Forget { count: 1 });
            }
            PatKind::Name(local) => {
                self.push(byc_instr::Reference {
                    at: self.depth.local as u32 - self.locals[local],
                });
                self.push(byc_instr::AssignCopyLhsThenRhs {});
                self.push(byc_instr::Forget { count: 1 });
            }
            PatKind::Tuple(tup) => {
                for (idx, pat) in tup.iter().enumerate() {
                    self.push(byc_instr::Tee { at: 0 });
                    self.push(byc_instr::TupleIndex { idx: idx as u32 });
                    self.lower_pat_assigns(pat.r(&self.tcx.session));
                }

                self.push(byc_instr::Forget { count: 1 });
            }
            PatKind::Error(_) => unreachable!(),
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
