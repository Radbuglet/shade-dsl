use crate::base::{Id, Symbol};

use super::{Func, FuncBlock, FuncExpr};

// === Values === //

/// A fully resolved compile-time value. Sub-components of this value (e.g. in types and functions)
/// may not be type-checked or even evaluated!
#[derive(Debug, Copy, Clone)]
pub struct Value<'gcx>(pub &'gcx ValueKind<'gcx>);

#[derive(Debug, Clone)]
pub enum ValueKind<'gcx> {
    /// A value representing a type. For instance, in...
    ///
    /// ```text
    /// const Foo = struct {};
    /// ```
    ///
    /// ...`Foo` would take on this value.
    ///
    /// Sub-items of this definition may not be fully resolved.
    ///
    /// The type of this value is [`MetaType`](TyKind::MetaType).
    MetaType(Ty<'gcx>),

    /// A function definition. For instance, in...
    ///
    /// ```text
    /// const my_func = fn(comptime foo: Ty1, bar: Ty2) -> Ty3 { ... }
    /// ```
    ///
    /// ...`my_func` would take on this value.
    ///
    /// This function may not be type-checked.
    ///
    /// The type of this value is [`MetaFunc`](TyKind::MetaFunc).
    MetaFunc(&'gcx Func<'gcx>),

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

#[derive(Debug, Copy, Clone)]
pub enum ValueRuntime<'gcx> {
    /// A scalar type (e.g. bool, integer, floating point number).
    ///
    /// The type of this value is [`Scalar`](TyRuntime::Scalar).
    Scalar(ValueScalar),

    /// An anonymous tuple of values.
    ///
    /// The type of this value is [`Tuple`](TyRuntime::Tuple).
    Tuple(&'gcx [Value<'gcx>]),

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
    /// The type of this value is [`AdtDef`](TyRuntime::AdtDef).
    Adt(&'gcx TyAdtDef<'gcx>, ValueAdt<'gcx>),
}

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

#[derive(Debug, Copy, Clone)]
pub enum ValueAdt<'gcx> {
    Uninhabited,
    Variant(u32, Value<'gcx>),
    Composite(&'gcx [Value<'gcx>]),
}

// === Types === //

#[derive(Debug, Copy, Clone)]
pub struct Ty<'gcx>(pub Id<'gcx, TyKind<'gcx>>);

#[derive(Debug, Clone)]
pub enum TyKind<'gcx> {
    /// The type of [`MetaType`](ValueKind::MetaType) values.
    MetaType,

    /// The type of [`MetaFunc`](ValueKind::MetaFunc) values.
    MetaFunc,

    /// The type of [`Runtime`](ValueKind::Runtime) values.
    ///
    /// The types contained in this enum will not contain [`Infer`] or [`Unresolved`] types since it
    /// would be impossible to construct a partially type-checked value.
    Runtime(TyRuntime<'gcx>),

    /// A type which has not yet been resolved. The resolution context for this type depends on
    /// where the type is defined. As such, users should be careful when dealing with types that
    /// contained unresolved sub-parts.
    Unresolved(Id<'gcx, FuncBlock<'gcx>>),

    /// A type which still needs to be inferred,
    Infer,
}

#[derive(Debug, Clone)]
pub struct TyAdtDef<'gcx> {
    pub kind: TyAdtKind,
    pub fields: &'gcx [TyAdtField<'gcx>],
    pub methods: &'gcx [TyAdtItem<'gcx>],
    pub statics: &'gcx [TyAdtItem<'gcx>],
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TyAdtKind {
    Module,
    Struct,
    TaggedUnion,
    UntaggedUnion,
}

#[derive(Debug, Copy, Clone)]
pub struct TyAdtItem<'gcx> {
    pub name: Symbol,
    pub init: Id<'gcx, FuncExpr<'gcx>>,
}

#[derive(Debug, Copy, Clone)]
pub struct TyAdtField<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

#[derive(Debug, Clone)]
pub enum TyRuntime<'gcx> {
    /// The type of [`Adt`](ValueRuntime::Adt) values.
    AdtDef(Id<'gcx, TyAdtDef<'gcx>>),

    /// The type of [`Tuple`](ValueRuntime::Tuple) values.
    Tuple(&'gcx [Ty<'gcx>]),

    /// The type of [`Scalar`](ValueRuntime::Scalar) values.
    Scalar(TyScalar),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
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
