//! The shade-DSL bytecode interpreter operates on a stack of places.

use crate::{
    base::arena::Obj,
    parse::ast::BinOpKind,
    typeck::syntax::{FuncInstance, Ty, ValuePlace},
};

#[derive(Debug)]
pub struct BycFunction {
    pub instance: Obj<FuncInstance>,
    pub instructions: Vec<BycInstr>,
}

#[derive(Debug)]
pub enum BycInstr {
    /// Reserves a new place and pushes it to the stack.
    Reserve,

    /// Allocates a `MetaType` value place and pushes it to the stack.
    AllocType(Obj<Ty>),

    /// Allocates a value by performing a deep-clone of an intern and pushes that to the stack.
    AllocConst(ValuePlace),

    /// Copies an existing place reference on the stack relative to the top and pushes it to the
    /// stack.
    Tee(u32),

    /// Pops the stack according to the pop-mode.
    Pop(BycPopMode),

    /// Evaluates a constant and performs a deep copy the resulting value, pushing a new place
    /// to the stack.
    ConstEval(Obj<FuncInstance>),

    /// Calls the function with the specified number of arguments. The stack layout is...
    ///
    /// ```text
    /// (top)
    /// - Arg N-1
    /// - ...
    /// - Arg 0
    /// - Callee
    /// ```
    ///
    /// This should be followed by a `CallCleanup` instruction.
    ///
    /// The callee returns to us with the return value at the top of the stack. Hence, this
    /// instruction "pushes" one item to the stack from the POV of the caller.
    CallStart(u32),

    /// Finishes up a function call started with `CallStart` by popping all arguments and callees
    /// off the stack using the `PopAndFree` pop mode and moves the function's result to the new top
    /// of the stack.
    CallCleanup(u32),

    /// Instantiates the function with the specified number of generics. The stack layout is...
    ///
    /// ```text
    /// (top)
    /// - Arg N-1
    /// - ...
    /// - Arg 0
    /// - Callee
    /// ```
    ///
    /// It then pops all arguments and the target with the `PopAndFree` pop mode and pushes the
    /// resolved function instance.
    Instantiate(u32),

    /// Returns to the caller.
    Return,

    /// Pops the supplied number of values from the stack assuming ownership is being transferred to
    /// us and produces a new tuple on the top of the stack.
    NewTuple(u32),

    /// Pops the supplied number of values from the stack using the `PopAndFree` pop mode and
    /// produces a new tuple type on the top of the stack.
    NewTupleType(u32),

    /// Pops the place at the top of the stack according to the pop mode and pushes the specified
    /// field inside the aggregate ADT onto the stack.
    AdtAggregateIndex(BycPopMode, u32),

    /// Pops the place at the top of the stack according to the pop mode and pushes the value inside
    /// the variant ADT onto the stack.
    AdtVariantUnwrap(BycPopMode),

    /// Pops the stack without freeing, allocates a new value with a shallow copy of the value, and
    /// pushes it to the stack.
    ShallowCopy,

    /// Pops the stack according to the first pop mode and uses it as an RHS, pops the stack again
    /// according to the second pop mode and uses it as an LHS, and performs the binary operation.
    BinOp(BycBinOp, BycPopMode, BycPopMode),

    /// Performs an unconditional jump.
    Jump(usize),

    /// Pops the stack according to the pop mode, expecting a boolean. If the boolean is false,
    /// jumps to the target instruction.
    JumpOtherwise(BycPopMode, usize),
}

impl BycInstr {
    pub fn depth_delta(&self) -> i32 {
        match *self {
            BycInstr::Reserve => 1,
            BycInstr::AllocType(..) => 1,
            BycInstr::AllocConst(..) => 1,
            BycInstr::Tee(..) => 1,
            BycInstr::Pop(..) => -1,
            BycInstr::ConstEval(..) => 1,
            BycInstr::CallStart(..) => 1,
            BycInstr::CallCleanup(args) => -(args as i32) - 1,
            BycInstr::Instantiate(generics) => -(generics as i32),
            BycInstr::Return => 0,
            BycInstr::NewTuple(fields) => -(fields as i32) + 1,
            BycInstr::NewTupleType(fields) => -(fields as i32) + 1,
            BycInstr::AdtAggregateIndex(..) => 0,
            BycInstr::AdtVariantUnwrap(..) => 0,
            BycInstr::ShallowCopy => 0,
            BycInstr::BinOp(..) => -1,
            BycInstr::Jump(..) => 0,
            BycInstr::JumpOtherwise(..) => 0,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum BycPopMode {
    JustPop,
    PopAndFree,
}

impl BycPopMode {
    pub fn needs_free(self) -> bool {
        self == BycPopMode::PopAndFree
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum BycBinOp {
    ShallowAssign,
    Scalar(BinOpKind),
}
