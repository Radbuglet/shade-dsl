//! The shade-DSL bytecode interpreter operates on a stack of places.

use crate::{
    base::arena::Obj,
    parse::ast::BinOpKind,
    typeck::syntax::{FuncInstance, ValueScalar},
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

    /// Allocates a scalar value place and pushes it to the stack.
    AllocScalar(Box<ValueScalar>),

    /// Copies an existing place reference on the stack relative to the top and pushes it to the
    /// stack.
    Tee(u32),

    /// Pops the stack according to the pop-mode.
    Pop(BycPopMode),

    /// Performs a deep copy of a constant value and pushes the new place to the stack.
    Const(Obj<FuncInstance>),

    /// Calls the function at the top of the stack, popping it according to the pop mode.
    Call(BycPopMode),

    /// Returns to the caller.
    Return,

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
        match self {
            BycInstr::Reserve => 1,
            BycInstr::AllocScalar(..) => 1,
            BycInstr::Tee(..) => 1,
            BycInstr::Pop(..) => -1,
            BycInstr::Const(..) => 1,
            BycInstr::Call(..) => -1,
            BycInstr::Return => 0,
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
