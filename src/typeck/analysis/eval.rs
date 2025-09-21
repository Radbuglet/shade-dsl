use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    typeck::{
        analysis::TyCtxt,
        syntax::{
            BycFunction, BycInstr, Func, FuncInstance, Ty, Value, ValueArena, ValueKind, ValuePlace,
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

    pub fn eval_ty(
        &self,
        ty: Obj<Func>,
        parent: Obj<FuncInstance>,
    ) -> Result<Obj<Ty>, ErrorGuaranteed> {
        let value = self.eval_paramless(self.intern_fn_instance(ty, Some(parent)))?;

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

        while let Some((curr_byc, curr_ip)) = call_stack.last().copied() {
            match curr_byc.instructions[curr_ip] {
                BycInstr::Allocate => {
                    place_stack.push(arena.reserve());
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
                BycInstr::Const(cst) => {
                    place_stack.push(arena.duplicate_from(
                        Some(self.value_interner.arena()),
                        self.eval_paramless(cst)?,
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

                    call_stack.push((self.build_bytecode(callee)?.r(s), 0usize));
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
                BycInstr::BinOp(op, rhs_mode, lhs_mode) => {
                    let rhs_place = place_stack.pop().unwrap();
                    let lhs_place = place_stack.pop().unwrap();

                    match op {
                        crate::typeck::syntax::BycBinOp::ShallowAssign => todo!(),
                        crate::typeck::syntax::BycBinOp::Scalar(bin_op_kind) => todo!(),
                    }

                    let to_copy = arena.read(place_stack.pop().unwrap()).clone();
                    *arena.mutate(place_stack.pop().unwrap()) = to_copy;
                }
                BycInstr::AssignLitScalar(ref value) => {
                    *arena.mutate(place_stack.pop().unwrap()) = Value {
                        ty: self.ty_interner.intern(Ty::Scalar(value.kind()), s),
                        kind: ValueKind::Scalar(**value),
                    };
                }
                BycInstr::Jump(_) => todo!(),
            }
        }

        debug_assert_eq!(place_stack.len(), 1);

        Ok(place_stack.pop().unwrap())
    }
}
