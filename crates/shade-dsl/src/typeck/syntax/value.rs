use std::hash;

use index_vec::IndexVec;

use crate::component;

use super::{ObjAdtSignature, ObjFunc, ObjTy, OwnConstIdx, OwnGenericIdx};

// === Values === //

#[derive(Debug)]
pub enum Value {
    MetaType(ObjTy),
    MetaFunc(FullInstance),
    Func(FullInstance),
    Scalar(ValueScalar),
    Tuple(Vec<ObjValue>),
    Adt(ObjAdtSignature, AdtValue),
}

component!(Value);

#[derive(Debug, Copy, Clone)]
pub enum ValueScalar {
    Bool(bool),
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

#[derive(Debug)]
pub enum AdtValue {
    Composite(Vec<ObjValue>),
    Variant(u32, ObjValue),
}

// === Instance === //

#[derive(Debug)]
pub struct PartialInstance {
    /// The function we're trying to instantiate.
    pub func: ObjFunc,

    /// The partial list of generic arguments to the function, filled from left to right.
    pub generics: Vec<ObjValue>,

    /// The upvars captured by parent functions which have already been evaluated.
    pub parent_results: Option<ObjFullInstance>,
}

#[derive(Debug)]
pub struct FullInstance {
    /// The function we're evaluating.
    pub func: ObjFunc,

    /// The lexical parent of this function, which may also be in the process of active evaluation.
    pub parent: Option<ObjFullInstance>,

    /// The generic parameters passed to the function.
    pub generics: IndexVec<OwnGenericIdx, ObjValue>,

    /// The constants we have evaluated thus far.
    pub consts: IndexVec<OwnConstIdx, Option<ObjValue>>,
}

component!(FullInstance);
