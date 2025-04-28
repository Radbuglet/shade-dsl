use crate::base::Symbol;

use super::Func;

// === Values === //

#[derive(Copy, Clone)]
pub struct Value<'gcx>(&'gcx ValueKind<'gcx>);

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
    Primitive(),
}

#[derive(Copy, Clone)]
pub enum ValueAdt<'gcx> {
    Uninhabited,
    Variant(u32, Value<'gcx>),
    Composite(&'gcx [Value<'gcx>]),
}

// === Types === //

#[derive(Copy, Clone)]
pub struct Ty<'gcx>(&'gcx TyKind<'gcx>);

pub enum TyKind<'gcx> {
    /// The type of [`MetaType`](ValueKind::MetaType) values.
    MetaType,

    /// The type of [`MetaFunc`](ValueKind::MetaFunc) values.
    MetaFunc,

    /// The type of [`Adt`](ValueKind::Adt) values.
    AdtDef(&'gcx TyAdtDef<'gcx>),

    /// The type of [`Primitive`](ValueKind::Primitive) values.
    Primitive(),
}

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
    pub ty: Ty<'gcx>,
    pub value: Value<'gcx>,
}

pub struct TyAdtField<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}
