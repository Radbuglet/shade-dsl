use crate::base::{Def, InternList, Symbol};

use super::{BoundTy, Ty, Value, ValueList};

// === Adts === //

pub type ItemList<'gcx> = InternList<'gcx, Item<'gcx>>;

pub type Item<'gcx> = Def<'gcx, ItemInner<'gcx>>;

pub struct ItemInner<'gcx> {
    pub name: Symbol,
    pub init: Instance<'gcx>,
}

// === Instance === //

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Instance<'gcx> {
    pub func: Func<'gcx>,
    pub generics: ValueList<'gcx>,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct BoundInstance<'gcx> {
    pub func: Func<'gcx>,
    pub generics: BoundValueList<'gcx>,
}

pub type BoundValueList<'gcx> = InternList<'gcx, BoundValue<'gcx>>;

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum BoundValue<'gcx> {
    Value(Value<'gcx>),
    Bound(FuncGeneric<'gcx>),
}

// === Functions === //

pub type Func<'gcx> = Def<'gcx, FuncInner<'gcx>>;

pub struct FuncInner<'gcx> {
    pub generics: &'gcx [FuncGeneric<'gcx>],
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub ret_type: BoundTy<'gcx>,
    pub main: FuncExpr<'gcx>,
}

pub type FuncGeneric<'gcx> = Def<'gcx, FuncGenericInner<'gcx>>;

pub struct FuncGenericInner<'gcx> {
    pub name: Symbol,
    pub ty: BoundTy<'gcx>,
}

pub type FuncLocal<'gcx> = Def<'gcx, FuncLocalInner<'gcx>>;

pub struct FuncLocalInner<'gcx> {
    pub name: Symbol,
    pub ty: BoundTy<'gcx>,
}

pub type FuncExpr<'gcx> = Def<'gcx, FuncExprInner<'gcx>>;

pub struct FuncExprInner<'gcx> {
    pub ty: Ty<'gcx>,
    pub kind: FuncExprKind<'gcx>,
}

pub enum FuncExprKind<'gcx> {
    /// Fetches the value of a local.
    Local(FuncLocal<'gcx>),

    /// Invokes the receiver with the specified arguments. The receiver must have type [`Func`].
    ///
    /// [`Func`]: super::TyRuntime::Func
    Call(FuncExpr<'gcx>, &'gcx [FuncExpr<'gcx>]),

    /// Evaluates the specified constant. The supplied instance must be `fully_specified` and the
    /// target function must take no arguments.
    Const(BoundInstance<'gcx>),

    /// Ascribes a specific type to an expression. Useful for inference.
    Ascribe(FuncExpr<'gcx>, Ty<'gcx>),

    /// Extends the argument list of a [`MetaFunc`] to contain yet another generic argument.
    ///
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    ProvideGeneric(FuncExpr<'gcx>, FuncExpr<'gcx>),

    /// Instantiates a [`MetaFunc`], transforming it into a [`Func`].
    ///
    /// [`Func`]: super::TyRuntime::Func
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    Instantiate(FuncExpr<'gcx>),

    /// Produces a [`MetaFunc`] from the specified [`Func`] literal
    ///
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    FuncLiteral(Func<'gcx>),

    /// Produces a [`MetaType`] from the specified type.
    ///
    /// [`MetaType`]: super::TyKind::MetaType
    TypeOf(BoundTy<'gcx>),

    /// Extends an existing [`MetaType`] with a new field and returns the resulting type.
    ///
    /// [`MetaType`]: super::TyKind::MetaType
    TypeWithField {
        target: FuncExpr<'gcx>,
        name: FuncExpr<'gcx>,
        ty: FuncExpr<'gcx>,
    },

    /// Extends an existing [`MetaType`] with a new method and returns the resulting type.
    ///
    /// `method_producer` is [`Func`] which takes in no arguments and returns a [`MetaFunc`].
    ///
    /// [`Func`]: super::TyRuntime::Func
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    /// [`MetaType`]: super::TyKind::MetaType
    TypeWithMethod {
        target: FuncExpr<'gcx>,
        name: FuncExpr<'gcx>,
        producer: FuncExpr<'gcx>,
    },

    /// Extends an existing [`MetaType`] with a new static and returns the resulting type.
    ///
    /// `method_producer` is [`Func`] which takes in no arguments and returns an arbitrary value.
    ///
    /// [`Func`]: super::TyRuntime::Func
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    /// [`MetaType`]: super::TyKind::MetaType
    TypeWithStatic {
        target: FuncExpr<'gcx>,
        name: FuncExpr<'gcx>,
        producer: FuncExpr<'gcx>,
    },
}
