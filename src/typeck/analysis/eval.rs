use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::{TyCtxt, WfRequirement},
        syntax::{
            AnyFuncValue, BycBinOp, BycFunction, BycInstr, CopyDepth, FuncInstance, Ty, Value,
            ValueArena, ValueKind, ValuePlace, ValueScalar,
        },
    },
};

impl TyCtxt {
    pub fn eval_paramless(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        self.queries.eval_paramless.compute(instance, |_| {
            let bytecode = self.build_bytecode(instance)?;
            let mut arena = ValueArena::default();
            let root = self.interpret(bytecode, &[], &mut arena)?;
            Ok(self.value_interner.intern(&arena, root))
        })
    }

    pub fn eval_ty(&self, instance: Obj<FuncInstance>) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let value = self.eval_paramless(instance)?;

        let ValueKind::MetaType(ty) = self.value_interner.read(value).kind else {
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
        let mut call_stack = Vec::from_iter([(root_func.r(s), 0usize)]);
        let mut place_stack = args.to_vec();

        while let Some((curr_byc, next_ip)) = call_stack.last_mut() {
            let curr_byc = *curr_byc;
            let curr_ip = *next_ip;
            *next_ip += 1;

            match curr_byc.instructions[curr_ip] {
                BycInstr::Reserve => {
                    place_stack.push(arena.reserve());
                }
                BycInstr::AllocScalar(ref value) => {
                    place_stack.push(arena.alloc(Value {
                        ty: self.intern_ty(Ty::Scalar(value.kind())),
                        kind: ValueKind::Scalar(**value),
                    }));
                }
                BycInstr::AllocType(ty) => {
                    if let Ty::Adt(instance) = ty.r(&self.session) {
                        self.queue_wf(WfRequirement::ValidateAdt(*instance));
                    }

                    place_stack.push(arena.alloc(Value {
                        ty: self.intern_ty(Ty::MetaTy),
                        kind: ValueKind::MetaType(ty),
                    }));
                }
                BycInstr::AllocConst(intern) => {
                    place_stack.push(arena.copy_from(
                        Some(self.value_interner.arena()),
                        intern,
                        CopyDepth::Deep,
                    ));
                }
                BycInstr::Tee(idx) => {
                    place_stack.push(place_stack[place_stack.len() - 1 - idx as usize]);
                }
                BycInstr::Pop(mode) => {
                    let top = place_stack.pop().unwrap();

                    if mode.needs_free() {
                        arena.free(top);
                    }
                }
                BycInstr::ConstEval(cst) => {
                    place_stack.push(arena.copy_from(
                        Some(self.value_interner.arena()),
                        self.eval_paramless(cst)?,
                        CopyDepth::Deep,
                    ));
                }
                BycInstr::Call(mode) => {
                    let callee_place = place_stack.pop().unwrap();
                    let ValueKind::Func(callee) = arena.read(callee_place).kind else {
                        unreachable!()
                    };

                    if mode.needs_free() {
                        arena.free(callee_place);
                    }

                    match callee {
                        AnyFuncValue::Intrinsic(callee) => todo!(),
                        AnyFuncValue::Instance(callee) => {
                            call_stack.push((self.build_bytecode(callee)?.r(s), 0usize));
                        }
                    }
                }
                BycInstr::Return => {
                    call_stack.pop().unwrap();
                }
                BycInstr::AdtVariantUnwrap(mode) => {
                    let place = place_stack.pop().unwrap();
                    let ValueKind::AdtVariant(_, inner) = arena.read(place).kind else {
                        unreachable!()
                    };

                    if mode.needs_free() {
                        arena.free(place);
                    }

                    place_stack.push(inner);
                }
                BycInstr::AdtAggregateIndex(mode, idx) => {
                    let place = place_stack.pop().unwrap();
                    let ValueKind::AdtAggregate(ref places) = arena.read(place).kind else {
                        unreachable!()
                    };

                    place_stack.push(places[idx as usize]);

                    if mode.needs_free() {
                        arena.free(place);
                    }
                }
                BycInstr::ShallowCopy => {
                    let to_copy = place_stack.pop().unwrap();
                    let copied = arena.copy(to_copy, CopyDepth::Shallow);
                    place_stack.push(copied);
                }
                BycInstr::BinOp(op, rhs_mode, lhs_mode) => {
                    let rhs_place = place_stack.pop().unwrap();
                    let lhs_place = place_stack.pop().unwrap();

                    match op {
                        BycBinOp::ShallowAssign => {
                            arena.assign(rhs_place, lhs_place);
                        }
                        BycBinOp::Scalar(op) => {
                            todo!()
                        }
                    }

                    if rhs_mode.needs_free() {
                        arena.free(rhs_place);
                    }

                    if lhs_mode.needs_free() {
                        arena.free(lhs_place);
                    }
                }
                BycInstr::Jump(addr) => {
                    *next_ip = addr;
                }
                BycInstr::JumpOtherwise(mode, addr) => {
                    let place = place_stack.pop().unwrap();
                    let ValueKind::Scalar(ValueScalar::Bool(value)) = arena.read(place).kind else {
                        unreachable!();
                    };

                    if !value {
                        *next_ip = addr;
                    }

                    if mode.needs_free() {
                        arena.free(place);
                    }
                }
            }
        }

        debug_assert_eq!(place_stack.len(), 1);

        Ok(place_stack.pop().unwrap())
    }
}
