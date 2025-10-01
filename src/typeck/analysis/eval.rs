use index_vec::IndexVec;

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::{TyCtxt, WfRequirement},
        syntax::{
            AnyFuncValue, AnyMetaFuncValue, BycBinOp, BycFunction, BycInstr, CopyDepth,
            FuncInstance, Ty, Value, ValueArena, ValueKind, ValuePlace, ValueScalar,
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

    pub fn ty_of_paramless_fn_val(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let value = self.eval_paramless(instance)?;

        Ok(self.value_interner.read(value).ty)
    }

    pub fn eval_paramless_for_meta_ty(
        &self,
        instance: Obj<FuncInstance>,
    ) -> Result<Obj<Ty>, ErrorGuaranteed> {
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

            let curr_instr = &curr_byc.instructions[curr_ip];

            let expected_depth = (place_stack.len() as u32)
                .checked_add_signed(curr_instr.depth_delta())
                .unwrap();

            match *curr_instr {
                BycInstr::Reserve => {
                    place_stack.push(arena.reserve());
                }
                BycInstr::AllocType(ty) => {
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
                BycInstr::CallStart(arg_count) => {
                    let callee_place = place_stack[place_stack.len() - arg_count as usize - 1];
                    let callee = arena.read(callee_place);

                    let ValueKind::Func(callee) = callee.kind else {
                        unreachable!()
                    };

                    match callee {
                        AnyFuncValue::Intrinsic(callee) => {
                            place_stack.push(callee.invoke(
                                self,
                                arena,
                                &place_stack[(place_stack.len() - arg_count as usize)..],
                            )?);
                        }
                        AnyFuncValue::Instance(callee) => {
                            call_stack.push((self.build_bytecode(callee)?.r(s), 0usize));
                        }
                    }
                }
                BycInstr::CallCleanup(arg_count) => {
                    let ret_place = place_stack.pop().unwrap();

                    for place in place_stack.drain((place_stack.len() - arg_count as usize - 1)..) {
                        arena.free(place);
                    }

                    place_stack.push(ret_place);
                }
                BycInstr::Instantiate(arg_count) => {
                    let target = place_stack[place_stack.len() - arg_count as usize - 1];
                    let args = &place_stack[(place_stack.len() - arg_count as usize)..];

                    let ValueKind::MetaFunc(target) = arena.read(target).kind else {
                        unreachable!();
                    };

                    let args = args
                        .iter()
                        .map(|&v| self.value_interner.intern(arena, v))
                        .collect::<IndexVec<_, _>>();

                    let resolved = match target {
                        AnyMetaFuncValue::Intrinsic(target) => {
                            let intern =
                                self.eval_intrinsic_meta_fn(target, &args.as_slice().raw)?;

                            arena.copy_from(
                                Some(self.value_interner.arena()),
                                intern,
                                CopyDepth::Deep,
                            )
                        }
                        AnyMetaFuncValue::Instance(target) => {
                            let expected_count = target.func.r(s).inner.generics.len();

                            if expected_count != args.len() {
                                todo!();
                            }

                            let instance =
                                self.intern_fn_instance_with(target.func, target.parent, args);

                            if target.func.r(s).inner.params.is_some() {
                                self.queue_wf(WfRequirement::TypeCheck(instance));

                                arena.alloc(Value {
                                    ty: self.instance_fn_ty(instance)?,
                                    kind: ValueKind::Func(AnyFuncValue::Instance(instance)),
                                })
                            } else {
                                let intern = self.eval_paramless(instance)?;

                                arena.copy_from(
                                    Some(self.value_interner.arena()),
                                    intern,
                                    CopyDepth::Deep,
                                )
                            }
                        }
                    };

                    for place in place_stack.drain((place_stack.len() - arg_count as usize - 1)..) {
                        arena.free(place);
                    }

                    place_stack.push(resolved);
                }
                BycInstr::Return => {
                    call_stack.pop().unwrap();
                }
                BycInstr::NewTuple(fields) => {
                    let args = &place_stack[(place_stack.len() - fields as usize)..];

                    let tuple_ty = self
                        .intern_tys(&args.iter().map(|v| arena.read(*v).ty).collect::<Vec<_>>());

                    let tuple_ty = self.intern_ty(Ty::Tuple(tuple_ty));

                    let tuple_val = arena.alloc(Value {
                        ty: tuple_ty,
                        kind: ValueKind::Tuple(args.to_vec()),
                    });

                    // Do not drop because we transferred ownership.
                    place_stack.truncate(place_stack.len() - args.len());
                    place_stack.push(tuple_val);
                }
                BycInstr::NewTupleType(arg_no) => {
                    let args = &place_stack[(place_stack.len() - arg_no as usize)..];

                    let args = self.intern_tys(
                        &args
                            .iter()
                            .map(|field| {
                                let ValueKind::MetaType(ty) = arena.read(*field).kind else {
                                    unreachable!();
                                };

                                ty
                            })
                            .collect::<Vec<_>>(),
                    );

                    let tuple_val = arena.alloc(Value {
                        ty: self.intern_ty(Ty::MetaTy),
                        kind: ValueKind::MetaType(self.intern_ty(Ty::Tuple(args))),
                    });

                    for place in place_stack.drain((place_stack.len() - arg_no as usize)..) {
                        arena.free(place);
                    }

                    place_stack.push(tuple_val);
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

            if !matches!(curr_instr, BycInstr::CallStart(_)) {
                debug_assert_eq!(
                    place_stack.len() as u32,
                    expected_depth,
                    "bad stack handling for {:?}",
                    curr_byc.instructions[curr_ip],
                );
            }
        }

        debug_assert_eq!(place_stack.len(), 1);

        Ok(place_stack.pop().unwrap())
    }
}
