use crate::base::Symbol;

use super::{Func, FuncBlock, FuncExpr};

// === Values === //

/// A fully resolved compile-time value. Values may contain `MetaType`s whose definitions are not
/// fully resolved, however.
#[derive(Copy, Clone)]
pub struct Value<'gcx>(pub &'gcx ValueKind<'gcx>);

#[derive(Clone)]
pub enum ValueKind<'gcx> {
    /// A value representing a type. For instance, in...
    ///
    /// ```text
    /// const Foo = struct {};
    /// ```
    ///
    /// ...`Foo` would take on this value.
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
    /// The type of this value is [`MetaFunc`](TyKind::MetaFunc).
    MetaFunc(&'gcx Func<'gcx>),

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
    /// The type of this value is [`AdtDef`](TyKind::AdtDef).
    Adt(&'gcx TyAdtDef<'gcx>, ValueAdt<'gcx>),

    /// An instantiation of a primitive. For example, in...
    ///
    /// ```text
    /// const FOO = 1;
    /// ```
    ///
    /// ...`FOO` would take on this value.
    ///
    /// The type of this value is [`Primitive`](TyKind::Primitive).
    Primitive(ValuePrimitive<'gcx>),
}

#[derive(Copy, Clone)]
pub enum ValueAdt<'gcx> {
    Uninhabited,
    Variant(u32, Value<'gcx>),
    Composite(&'gcx [Value<'gcx>]),
}

#[derive(Copy, Clone)]
pub enum ValuePrimitive<'gcx> {
    Tuple(&'gcx [Value<'gcx>]),
    Scalar(ValueScalar),
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

// === Types === //

#[derive(Copy, Clone)]
pub struct Ty<'gcx>(pub &'gcx TyKind<'gcx>);

#[derive(Clone)]
pub enum TyKind<'gcx> {
    /// The type of [`MetaType`](ValueKind::MetaType) values.
    MetaType,

    /// The type of [`MetaFunc`](ValueKind::MetaFunc) values.
    MetaFunc,

    /// The type of [`Adt`](ValueKind::Adt) values.
    AdtDef(&'gcx TyAdtDef<'gcx>),

    /// The type of [`Primitive`](ValueKind::Primitive) values.
    Primitive(TyPrimitive<'gcx>),

    /// A type which has not yet been resolved. The resolution context for this type depends on
    /// where the type is defined. As such, users should be careful when dealing with types that
    /// contained unresolved sub-parts.
    Unresolved(&'gcx FuncBlock<'gcx>),

    /// A type which still needs to be inferred,
    Infer,
}

#[derive(Clone)]
pub struct TyAdtDef<'gcx> {
    pub kind: TyAdtKind,
    pub fields: &'gcx [TyAdtField<'gcx>],
    pub methods: &'gcx [TyAdtItem<'gcx>],
    pub statics: &'gcx [TyAdtItem<'gcx>],
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum TyAdtKind {
    Module,
    Struct,
    TaggedUnion,
    UntaggedUnion,
}

#[derive(Copy, Clone)]
pub struct TyAdtItem<'gcx> {
    pub name: Symbol,
    pub init: &'gcx FuncExpr<'gcx>,
}

#[derive(Copy, Clone)]
pub struct TyAdtField<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

#[derive(Clone)]
pub enum TyPrimitive<'gcx> {
    Tuple(&'gcx [Ty<'gcx>]),
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
