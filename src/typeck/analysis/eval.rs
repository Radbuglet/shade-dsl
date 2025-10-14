use index_vec::IndexVec;

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::BinOpKind,
    typeck::{
        analysis::{TyCtxt, WfRequirement},
        syntax::{
            AnyFuncValue, AnyMetaFuncValue, BycDepth, BycFunction, BycInstr, BycInstrHandler,
            CopyDepth, FuncInstance, TaggedTy, Ty, TyTag, ValueArena, ValueKind, ValuePlace,
            ValueScalar, byc_instr,
        },
    },
};

// TODO: Improve type error diagnostics since type-punning can cause them.
impl TyCtxt {
    pub fn eval_paramless(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        self.queries.eval_paramless.compute(instance, |_| {
            let bytecode = self.build_bytecode(instance)?;
            let mut arena = ValueArena::new(self.clone());
            let root = self.interpret(bytecode, &[], &mut arena)?;
            Ok(self.value_interner.intern(&arena, root))
        })
    }

    pub fn eval_paramless_unwrap_any(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        let mut out = self.eval_paramless(instance)?;

        while let ValueKind::MetaAny(Some(curr)) = self.value_interner.read(out).kind {
            out = curr;
        }

        Ok(out)
    }

    pub fn tagged_ty_for_eval_paramless_unwrap_any(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<TaggedTy, ErrorGuaranteed> {
        let value = self.eval_paramless_unwrap_any(instance)?;

        Ok(self.tagged_ty_from_intern(value))
    }

    pub fn tagged_ty_from_intern(&self, intern: ValuePlace) -> TaggedTy {
        let intern = self.value_interner.read(intern);

        TaggedTy {
            ty: intern.ty,
            tag: match &intern.kind {
                ValueKind::MetaType(Some(ty)) => TyTag::MetaTyKnown(*ty),
                _ => TyTag::Nothing,
            },
        }
    }

    pub fn eval_paramless_for_meta_ty(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let value = self.eval_paramless(instance)?;

        let ValueKind::MetaType(Some(ty)) = self.value_interner.read(value).kind else {
            todo!();
        };

        Ok(ty)
    }

    pub fn interpret(
        &self,
        root_func: Obj<BycFunction>,
        args: &[ValuePlace],
        arena: &mut ValueArena,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        let s = &self.session;

        let facts = self.type_check(root_func.r(s).instance).unwrap().r(s);
        let return_place = arena.reserve(facts.return_ty);

        let place_stack = [return_place, ValuePlace::DANGLING]
            .into_iter()
            .chain(args.iter().map(|intern| {
                arena.copy_from(Some(self.value_interner.arena()), *intern, CopyDepth::Deep)
            }))
            .collect::<Vec<_>>();

        InterpretCx {
            tcx: self,
            arena,
            call_stack: Vec::from_iter([(root_func.r(s), 0usize)]),
            local_stack: Vec::new(),
            place_stack,
        }
        .run()?;

        Ok(return_place)
    }
}

struct InterpretCx<'a> {
    tcx: &'a TyCtxt,
    arena: &'a mut ValueArena,
    call_stack: Vec<(&'a BycFunction, usize)>,
    local_stack: Vec<ValuePlace>,
    place_stack: Vec<ValuePlace>,
}

impl InterpretCx<'_> {
    pub fn run(&mut self) -> Result<(), ErrorGuaranteed> {
        while let Some((curr_byc, next_ip)) = self.call_stack.last_mut() {
            let curr_byc = *curr_byc;
            let curr_ip = *next_ip;
            *next_ip += 1;

            let curr_instr = &curr_byc.instructions[curr_ip];

            let expected_depth = BycDepth {
                local: self.local_stack.len() as i32,
                place: self.place_stack.len() as i32,
            } + curr_instr.depth_delta();

            curr_instr.invoke(
                &mut self.local_stack,
                &mut self.place_stack,
                &mut InterpretCxHandler {
                    tcx: self.tcx,
                    arena: &mut *self.arena,
                    call_stack: &mut self.call_stack,
                },
            )?;

            if !matches!(curr_instr, BycInstr::Call(_)) {
                debug_assert_eq!(
                    expected_depth,
                    BycDepth {
                        local: self.local_stack.len() as i32,
                        place: self.place_stack.len() as i32,
                    },
                    "bad stack handling for {:?}",
                    curr_byc.instructions[curr_ip],
                );
            }
        }

        Ok(())
    }
}

struct InterpretCxHandler<'tcx, 'a> {
    tcx: &'tcx TyCtxt,
    arena: &'a mut ValueArena,
    call_stack: &'a mut Vec<(&'tcx BycFunction, usize)>,
}

impl InterpretCxHandler<'_, '_> {
    fn set_next_ip(&mut self, addr: usize) {
        self.call_stack.last_mut().unwrap().1 = addr;
    }
}

#[allow(clippy::needless_lifetimes)]
impl BycInstrHandler for InterpretCxHandler<'_, '_> {
    fn allocate<'a>(
        &mut self,
        instr: &'a byc_instr::Allocate,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        Ok([self.arena.reserve(instr.ty)])
    }

    fn deallocate<'a>(
        &mut self,
        _instr: &'a byc_instr::Deallocate,
        to_free: &'a [ValuePlace],
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        for &place in to_free {
            self.arena.free(place);
        }

        Ok([])
    }

    fn reference<'a>(
        &mut self,
        _instr: &'a byc_instr::Reference,
        _ignore: &'a [ValuePlace],
        target: ValuePlace,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        Ok([target])
    }

    fn tee<'a>(
        &mut self,
        _instr: &'a byc_instr::Tee,
        _ignore: &'a [ValuePlace],
        target: ValuePlace,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        Ok([target])
    }

    fn fold<'a>(
        &mut self,
        _instr: &'a byc_instr::Fold,
        top: ValuePlace,
        _drop: &'a [ValuePlace],
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        Ok([top])
    }

    fn forget<'a>(
        &mut self,
        _instr: &'a byc_instr::Forget,
        _ignore: &'a [ValuePlace],
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        Ok([])
    }

    fn assign_type_literal<'a>(
        &mut self,
        instr: &'a byc_instr::AssignTypeLiteral,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        self.arena
            .write_terminal(target, ValueKind::MetaType(Some(instr.ty)));

        Ok([])
    }

    fn assign_const<'a>(
        &mut self,
        instr: &'a byc_instr::AssignConst,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let temp = self.arena.copy_from(
            Some(self.tcx.value_interner.arena()),
            instr.intern,
            CopyDepth::Deep,
        );

        self.arena.assign(temp, target);
        self.arena.free(temp);

        Ok([])
    }

    fn assign_const_expr<'a>(
        &mut self,
        instr: &'a byc_instr::AssignConstExpr,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let intern = self.tcx.eval_paramless_unwrap_any(instr.func)?;

        let temp = self.arena.copy_from(
            Some(self.tcx.value_interner.arena()),
            intern,
            CopyDepth::Deep,
        );

        self.arena.assign(temp, target);
        self.arena.free(temp);

        Ok([])
    }

    fn assign_tuple_type<'a>(
        &mut self,
        _instr: &'a byc_instr::AssignTupleType,
        fields: &'a [ValuePlace],
        assign_to: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let ty = self.tcx.intern_tys(
            &fields
                .iter()
                .map(|place| {
                    let ValueKind::MetaType(Some(ty)) = self.arena.read(*place).kind else {
                        unreachable!()
                    };

                    ty
                })
                .collect::<Vec<_>>(),
        );
        let ty = self.tcx.intern_ty(Ty::Tuple(ty));

        self.arena
            .write_terminal(assign_to, ValueKind::MetaType(Some(ty)));

        Ok([])
    }

    fn assign_copy_lhs_then_rhs<'a>(
        &mut self,
        _instr: &'a byc_instr::AssignCopyLhsThenRhs,
        lhs: ValuePlace,
        rhs: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        self.arena.assign(rhs, lhs);

        Ok([])
    }

    fn assign_copy_rhs_then_lhs<'a>(
        &mut self,
        _instr: &'a byc_instr::AssignCopyRhsThenLhs,
        rhs: ValuePlace,
        lhs: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        self.arena.assign(rhs, lhs);

        Ok([])
    }

    fn call<'a>(
        &mut self,
        _instr: &'a byc_instr::Call,
        args: &'a [ValuePlace],
        callee: ValuePlace,
        return_to: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let callee = self.arena.read(callee);

        let ValueKind::Func(Some(callee)) = callee.kind else {
            unreachable!()
        };

        match callee {
            AnyFuncValue::Intrinsic(callee) => {
                let temp = callee.invoke(
                    self.tcx,
                    self.arena,
                    &args.iter().copied().rev().collect::<Vec<_>>(),
                )?;

                self.arena.assign(temp, return_to);
                self.arena.free(temp);
            }
            AnyFuncValue::Instance(callee) => {
                self.call_stack.push((
                    self.tcx.build_bytecode(callee)?.r(&self.tcx.session),
                    0usize,
                ));
            }
        }

        Ok([])
    }

    fn instantiate<'a>(
        &mut self,
        _instr: &'a byc_instr::Instantiate,
        args: &'a [ValuePlace],
        target: ValuePlace,
        write_to: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let ValueKind::MetaFunc(Some(target)) = self.arena.read(target).kind else {
            unreachable!();
        };

        let args = args
            .iter()
            .map(|&v| self.tcx.value_interner.intern(self.arena, v))
            .collect::<IndexVec<_, _>>();

        let temp = match target {
            AnyMetaFuncValue::Intrinsic(target) => {
                let intern = self
                    .tcx
                    .eval_intrinsic_meta_fn(target, &args.as_slice().raw)?;

                self.arena.copy_from(
                    Some(self.tcx.value_interner.arena()),
                    intern,
                    CopyDepth::Deep,
                )
            }
            AnyMetaFuncValue::Instance(target) => {
                let expected_count = target.func.r(&self.tcx.session).inner.generics.len();

                if expected_count != args.len() {
                    todo!();
                }

                let instance = self
                    .tcx
                    .intern_fn_instance_with(target.func, target.parent, args);

                if target.func.r(&self.tcx.session).inner.params.is_some() {
                    self.tcx.queue_wf(WfRequirement::TypeCheck(instance));

                    let temp = self.arena.reserve(self.tcx.instance_fn_ty(instance)?);
                    self.arena.write_terminal(
                        temp,
                        ValueKind::Func(Some(AnyFuncValue::Instance(instance))),
                    );
                    temp
                } else {
                    let intern = self.tcx.eval_paramless(instance)?;

                    self.arena.copy_from(
                        Some(self.tcx.value_interner.arena()),
                        intern,
                        CopyDepth::Deep,
                    )
                }
            }
        };

        let write_to = self
            .arena
            .write_meta_any_some(write_to, self.arena.read(temp).ty);

        self.arena.assign(temp, write_to);
        self.arena.free(temp);

        Ok([])
    }

    fn return_<'a>(
        &mut self,
        _instr: &'a byc_instr::Return,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        self.call_stack.pop().unwrap();

        Ok([])
    }

    fn adt_aggregate_index<'a>(
        &mut self,
        instr: &'a byc_instr::AdtAggregateIndex,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        let ValueKind::AdtAggregate(fields) = &self.arena.read(target).kind else {
            unreachable!();
        };

        Ok([fields[instr.idx as usize]])
    }

    fn tuple_index<'a>(
        &mut self,
        instr: &'a byc_instr::TupleIndex,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        let ValueKind::Tuple(fields) = &self.arena.read(target).kind else {
            unreachable!();
        };

        Ok([fields[instr.idx as usize]])
    }

    fn adt_variant_unwrap<'a>(
        &mut self,
        _instr: &'a byc_instr::AdtVariantUnwrap,
        target: ValuePlace,
    ) -> Result<[ValuePlace; 1], ErrorGuaranteed> {
        let ValueKind::AdtVariant(Some((_variant, inner))) = self.arena.read(target).kind else {
            unreachable!();
        };

        Ok([inner])
    }

    fn bin_op<'a>(
        &mut self,
        instr: &'a byc_instr::BinOp,
        rhs: ValuePlace,
        lhs: ValuePlace,
        assign_to: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        match instr.op {
            BinOpKind::Add => todo!(),
            BinOpKind::Sub => todo!(),
            BinOpKind::Mul => todo!(),
            BinOpKind::Div => todo!(),
            BinOpKind::Mod => todo!(),
            BinOpKind::Pow => todo!(),
            BinOpKind::BitAnd => todo!(),
            BinOpKind::BitOr => todo!(),
            BinOpKind::BitXor => todo!(),
            BinOpKind::LogicalAnd => todo!(),
            BinOpKind::LogicalOr => todo!(),
            BinOpKind::Eq => todo!(),
        }
    }

    fn jump<'a>(&mut self, instr: &'a byc_instr::Jump) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        self.set_next_ip(instr.addr);

        Ok([])
    }

    fn jump_otherwise<'a>(
        &mut self,
        instr: &'a byc_instr::JumpOtherwise,
        scrutinee: ValuePlace,
    ) -> Result<[ValuePlace; 0], ErrorGuaranteed> {
        let ValueKind::Scalar(Some(ValueScalar::Bool(value))) = self.arena.read(scrutinee).kind
        else {
            unreachable!();
        };

        if !value {
            self.set_next_ip(instr.addr);
        }

        Ok([])
    }
}
