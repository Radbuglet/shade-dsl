use std::{hash, panic::Location};

use derive_where::derive_where;
use index_vec::IndexVec;

use crate::{
    base::{ErrorGuaranteed, Session, arena::Obj},
    typeck::{
        analysis::TyCtxt,
        syntax::{ExprAdt, Func, ValueArena},
    },
};

use super::OwnGenericIdx;

// === Handles === //

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
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
    MetaFunc(AnyMetaFuncValue),

    /// An array of values whose size is not part of its type.
    MetaArray(Vec<ValuePlace>),

    /// A pointer to another value.
    Pointer(ValuePlace),

    /// A value representing an instantiated function.
    Func(AnyFuncValue),

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

/// A superset of types for use in type-checking. Values never have these type—they are only ever a
/// concept for type-checkers.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CheckTy {
    Regular(Obj<Ty>),

    /// The type of an integer literal whose actual type has yet to be inferred.
    InferInt,

    /// The type of a float literal whose actual type has yet to be inferred.
    InferFloat,

    /// The type of an expression is not known until evaluation time.
    ///
    /// Meta function instantiation, for example, will cause this type to be recorded.
    ///
    /// Functions without defined return values that return lexically unknown types will resolve to
    /// the actual type of the value. For example, in `const MyVec = Vec.<u32>;`, `MyVec` will have
    /// `MetaType` rather than `Unknown` because that's what the underlying value was at
    /// evaluation time.
    Unknown,
}

pub type TyList = Obj<[Obj<Ty>]>;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Ty {
    MetaTy,
    MetaFunc,
    MetaArray(Obj<Ty>),
    Pointer(Obj<Ty>),
    Func(TyList, Obj<Ty>),
    Scalar(ScalarKind),
    Tuple(TyList),
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

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyFuncValue {
    Intrinsic(FuncIntrinsic),
    Instance(Obj<FuncInstance>),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyMetaFuncValue {
    Intrinsic(MetaFuncIntrinsic),
    Instance(MetaFuncInstance),
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

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct MetaFuncInstance {
    /// The function we're trying to instantiate.
    pub func: Obj<Func>,

    /// The upvars captured by parent functions which have already been evaluated.
    pub parent: Option<Obj<FuncInstance>>,
}

// === Intrinsic Functions === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct MetaFuncIntrinsic {
    inner: Obj<IntrinsicMetaFnInner>,
}

#[derive_where(Debug)]
struct IntrinsicMetaFnInner {
    _location: &'static Location<'static>,

    #[derive_where(skip)]
    #[expect(clippy::type_complexity)]
    construct: Box<dyn Fn(&TyCtxt, &[ValuePlace]) -> Result<FuncIntrinsic, ErrorGuaranteed>>,
}

impl MetaFuncIntrinsic {
    #[track_caller]
    pub fn new(
        f: impl 'static + Fn(&TyCtxt, &[ValuePlace]) -> Result<FuncIntrinsic, ErrorGuaranteed>,
        s: &Session,
    ) -> Self {
        Self {
            inner: Obj::new(
                IntrinsicMetaFnInner {
                    _location: Location::caller(),
                    construct: Box::new(f),
                },
                s,
            ),
        }
    }

    pub fn instance_uncached(
        &self,
        tcx: &TyCtxt,
        args: &[ValuePlace],
    ) -> Result<FuncIntrinsic, ErrorGuaranteed> {
        (self.inner.r(&tcx.session).construct)(tcx, args)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct FuncIntrinsic {
    inner: Obj<FuncIntrinsicInner>,
}

#[derive_where(Debug)]
struct FuncIntrinsicInner {
    _location: &'static Location<'static>,

    #[derive_where(skip)]
    #[expect(clippy::type_complexity)]
    invoke:
        Box<dyn Fn(&TyCtxt, &mut ValueArena, &[ValuePlace]) -> Result<ValuePlace, ErrorGuaranteed>>,
}

impl FuncIntrinsic {
    #[track_caller]
    pub fn new<F>(f: F, s: &Session) -> Self
    where
        F: Fn(&TyCtxt, &mut ValueArena, &[ValuePlace]) -> Result<ValuePlace, ErrorGuaranteed>,
        F: 'static,
    {
        Self {
            inner: Obj::new(
                FuncIntrinsicInner {
                    _location: Location::caller(),
                    invoke: Box::new(f),
                },
                s,
            ),
        }
    }

    pub fn invoke(
        &self,
        tcx: &TyCtxt,
        arena: &mut ValueArena,
        args: &[ValuePlace],
    ) -> Result<ValuePlace, ErrorGuaranteed> {
        (self.inner.r(&tcx.session).invoke)(tcx, arena, args)
    }
}
