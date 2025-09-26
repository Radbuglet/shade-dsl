use std::hash;

use index_vec::IndexVec;

use crate::{
    base::arena::Obj,
    typeck::syntax::{ExprAdt, Func},
};

use super::OwnGenericIdx;

// === Handles === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct ValuePlace(pub(super) thunderdome::Index);

// === Values === //

#[derive(Debug, Clone)]
pub struct Value {
    pub ty: Obj<Ty>,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    /// A value representing a type.
    MetaType(Obj<Ty>),

    /// A value representing an uninstantiated function.
    MetaFunc(SemiFuncInstance),

    /// An array of values whose size is not part of its type.
    MetaArray(Vec<ValuePlace>),

    /// A pointer to another value.
    Pointer(ValuePlace),

    /// A value representing an instantiated function.
    Func(Obj<FuncInstance>),

    /// A value representing a scalar.
    Scalar(ValueScalar),

    /// A value representing a tuple.
    Tuple(Vec<ValuePlace>),

    /// A value representing a statically-allocated array of values.
    Array(Vec<ValuePlace>),

    /// A value representing a user-defined aggregate ADT.
    AdtAggregate(Vec<ValuePlace>),

    /// A value representing a user-defined variant ADT.
    AdtVariant(u32, ValuePlace),
}

#[derive(Debug, Copy, Clone)]
pub enum ValueScalar {
    Bool(bool),
    Char(char),
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    U128(u128),
    I128(i128),
    F32(f32),
    F64(f64),
    USize(usize),
    ISize(isize),
}

impl hash::Hash for ValueScalar {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            ValueScalar::Bool(v) => v.hash(state),
            ValueScalar::Char(v) => v.hash(state),
            ValueScalar::U8(v) => v.hash(state),
            ValueScalar::I8(v) => v.hash(state),
            ValueScalar::U16(v) => v.hash(state),
            ValueScalar::I16(v) => v.hash(state),
            ValueScalar::U32(v) => v.hash(state),
            ValueScalar::I32(v) => v.hash(state),
            ValueScalar::U64(v) => v.hash(state),
            ValueScalar::I64(v) => v.hash(state),
            ValueScalar::U128(v) => v.hash(state),
            ValueScalar::I128(v) => v.hash(state),
            ValueScalar::F32(v) => v.to_bits().hash(state),
            ValueScalar::F64(v) => v.to_bits().hash(state),
            ValueScalar::USize(v) => v.hash(state),
            ValueScalar::ISize(v) => v.hash(state),
        }
    }
}

impl Eq for ValueScalar {}

impl PartialEq for ValueScalar {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Char(lhs), Self::Char(rhs)) => lhs == rhs,
            (Self::U8(lhs), Self::U8(rhs)) => lhs == rhs,
            (Self::I8(lhs), Self::I8(rhs)) => lhs == rhs,
            (Self::U16(lhs), Self::U16(rhs)) => lhs == rhs,
            (Self::I16(lhs), Self::I16(rhs)) => lhs == rhs,
            (Self::U32(lhs), Self::U32(rhs)) => lhs == rhs,
            (Self::I32(lhs), Self::I32(rhs)) => lhs == rhs,
            (Self::U64(lhs), Self::U64(rhs)) => lhs == rhs,
            (Self::I64(lhs), Self::I64(rhs)) => lhs == rhs,
            (Self::U128(lhs), Self::U128(rhs)) => lhs == rhs,
            (Self::I128(lhs), Self::I128(rhs)) => lhs == rhs,
            (Self::F32(lhs), Self::F32(rhs)) => lhs == rhs,
            (Self::F64(lhs), Self::F64(rhs)) => lhs == rhs,
            (Self::USize(lhs), Self::USize(rhs)) => lhs == rhs,
            (Self::ISize(lhs), Self::ISize(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

// === Ty === //

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Ty {
    MetaTy,
    MetaFunc,
    MetaArray(Obj<Ty>),
    Pointer(Obj<Ty>),
    Func(Vec<Obj<Ty>>, Obj<Ty>),
    Scalar(ScalarKind),
    Tuple(Vec<Obj<Ty>>),
    Array(Obj<Ty>, usize),
    Adt(AdtInstance),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum ScalarKind {
    Bool,
    Char,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    F32,
    F64,
    USize,
    ISize,
}

// === Instance === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct AdtInstance {
    /// The function which created the ADT.
    pub owner: Obj<FuncInstance>,

    /// The structure being instantiated.
    pub adt: Obj<ExprAdt>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FuncInstance {
    /// The function we're evaluating.
    pub func: Obj<Func>,

    /// The lexical parent of this function, which may also be in the process of active evaluation.
    pub parent: Option<Obj<FuncInstance>>,

    /// The generic parameters passed to the function.
    pub generics: IndexVec<OwnGenericIdx, ValuePlace>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct SemiFuncInstance {
    /// The function we're trying to instantiate.
    pub func: Obj<Func>,

    /// The upvars captured by parent functions which have already been evaluated.
    pub parent: Option<Obj<FuncInstance>>,

    /// The partial list of generic arguments to the function, filled from left to right.
    pub generics: Vec<ValuePlace>,
}
