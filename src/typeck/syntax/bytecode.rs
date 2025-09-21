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
    Allocate,

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

    /// Pops the stack according to the first pop mode and uses it as an RHS, pops the stack again
    /// according to the second pop mode and uses it as an LHS, and performs the binary operation.
    BinOp(BycBinOp, BycPopMode, BycPopMode),

    /// Sets a place to a scalar value literal.
    AssignLitScalar(Box<ValueScalar>),

    /// Performs an unconditional jump.
    Jump(i32),
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
