use std::hash;

use crate::base::{Intern, InternList, Symbol};

use super::{BoundInstance, Func, Instance, ItemList};

// === Values === //

pub type Value<'gcx> = Intern<'gcx, ValueKind<'gcx>>;
pub type ValueList<'gcx> = InternList<'gcx, Value<'gcx>>;

/// A fully resolved compile-time value. Sub-components of this value (e.g. in types and functions)
/// may not be type-checked or even evaluated!
#[derive(Clone, Hash, Eq, PartialEq)]
pub enum ValueKind<'gcx> {
    /// A value representing a type. For instance, in...
    ///
    /// ```text
    /// const Foo = struct {};
    /// ```
    ///
    /// ...`Foo` would take on this value.
    ///
    /// Sub-items of this definition may not be fully resolved or type-checked.
    ///
    /// The type of this value is [`MetaType`](TyKind::MetaType). The type `0` would be of type
    /// [`TyRuntime::Adt`].
    MetaType(Ty<'gcx>),

    /// A function definition with uninstantiated generic parameters. For instance, in...
    ///
    /// ```text
    /// const my_func = fn<foo: Ty1>(bar: Ty2) -> Ty3 { ... }
    /// ```
    ///
    /// ...`my_func` would take on this value.
    ///
    /// This function may not be type-checked.
    ///
    /// The type of this value is [`MetaFunc`](TyKind::MetaFunc).
    MetaFunc(Func<'gcx>),

    /// An instantiation of a runtime value. For instance, in...
    ///
    /// ```text
    /// const MY_CONST = 1;
    /// ```
    ///
    /// ...`MY_CONST` would take on this value.
    ///
    /// The type of this value is [`Runtime`](TyKind::Runtime).
    Runtime(ValueRuntime<'gcx>),
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum ValueRuntime<'gcx> {
    /// A reified function with all generic parameters resolved.
    ///
    /// This function may not be type-checked.
    ///
    /// The type of this value is [`Func`](TyRuntime::Func).
    Func(Instance<'gcx>),

    /// A scalar type (e.g. bool, integer, floating point number).
    ///
    /// The type of this value is [`Scalar`](TyRuntime::Scalar).
    Scalar(ValueScalar),

    /// An anonymous tuple of values.
    ///
    /// The type of this value is [`Tuple`](TyRuntime::Tuple).
    Tuple(ValueList<'gcx>),

    /// An instantiation of an abstract data type. For example, in...
    ///
    /// ```text
    /// const Foo = struct { a: u32, b: i32 };
    ///
    /// const MY_FOO = Foo { a: 1, b: 2 };
    /// ```
    ///
    /// ...`MY_FOO` would take on this value.
    ///
    /// The type of this value is [`Adt`](TyRuntime::Adt).
    Adt(TyAdtSignature<'gcx>, ValueAdt<'gcx>),
}

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum ValueAdt<'gcx> {
    Uninhabited,
    Variant(u32, Value<'gcx>),
    Composite(ValueList<'gcx>),
}

// === Types === //

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct InferVar(pub u64);

pub type BoundTyList<'gcx> = InternList<'gcx, BoundTy<'gcx>>;

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum BoundTy<'gcx> {
    Unbound(Ty<'gcx>),
    Bound(BoundInstance<'gcx>),
}

pub type Ty<'gcx> = Intern<'gcx, TyKind<'gcx>>;
pub type TyList<'gcx> = InternList<'gcx, TyKind<'gcx>>;

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum TyKind<'gcx> {
    /// The type of [`MetaType`](ValueKind::MetaType) values.
    MetaType,

    /// The type of [`MetaFunc`](ValueKind::MetaFunc) values.
    MetaFunc,

    /// The type of [`Runtime`](ValueKind::Runtime) values.
    Runtime(TyRuntime<'gcx>),

    /// A type which has yet to be evaluated. To evaluate this type, the target `fully_specified`
    /// instance is evaluated with no arguments in order to produce the [`MetaType`] used to
    /// concretize this type.
    ///
    /// In general, we try to evaluate types as late as possible to allow cyclic type definitions to
    /// work.
    ///
    /// [`MetaType`]: ValueKind::MetaType
    Unevaluated(Instance<'gcx>),

    /// A yet-to-be-inferred type which has not yet been given an inference variable. Each inference
    /// variable is unique throughout the entire compilation session but lexically defined.
    Infer(InferVar),
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum TyRuntime<'gcx> {
    /// A reified function with all generic parameters instantiated.
    Func(TyList<'gcx>, Ty<'gcx>),

    /// The type of [`Adt`](ValueRuntime::Adt) values.
    Adt(TyAdtSignature<'gcx>),

    /// The type of [`Tuple`](ValueRuntime::Tuple) values.
    Tuple(TyList<'gcx>),

    /// The type of [`Scalar`](ValueRuntime::Scalar) values.
    Scalar(TyScalar),
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum TyScalar {
    Bool,
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

pub type TyAdtSignature<'gcx> = Intern<'gcx, TyAdtSignatureInner<'gcx>>;

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct TyAdtSignatureInner<'gcx> {
    pub kind: TyAdtKind,
    pub field_names: InternList<'gcx, Symbol>,
    pub field_types: TyList<'gcx>,
    pub methods: ItemList<'gcx>,
    pub statics: ItemList<'gcx>,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum TyAdtKind {
    Module,
    Struct,
    TaggedUnion,
    UntaggedUnion,
}
