use crate::base::{mem::Def, syntax::Symbol};

use super::{BoundInstance, Ty, TyList};

pub type Func<'gcx> = Def<'gcx, FuncInner<'gcx>>;

pub struct FuncInner<'gcx> {
    pub generics: &'gcx [FuncGeneric<'gcx>],
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub argument_types: TyList<'gcx>,
    pub ret_type: Ty<'gcx>,
    pub main: FuncExpr<'gcx>,
}

pub type FuncGeneric<'gcx> = Def<'gcx, FuncGenericInner<'gcx>>;

pub struct FuncGenericInner<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

pub type FuncLocal<'gcx> = Def<'gcx, FuncLocalInner>;

pub struct FuncLocalInner {
    pub name: Symbol,
}

pub type FuncExpr<'gcx> = Def<'gcx, FuncExprKind<'gcx>>;

pub enum FuncExprKind<'gcx> {
    /// Fetches the value of a local.
    Local(FuncLocal<'gcx>),

    /// Invokes the receiver with the specified arguments. The receiver must have type [`Func`].
    ///
    /// [`Func`]: super::TyKind::Func
    Call(FuncExpr<'gcx>, &'gcx [FuncExpr<'gcx>]),

    /// Evaluates the specified constant. The supplied instance must be `fully_specified` and the
    /// target function must take no arguments.
    Const(BoundInstance<'gcx>),

    /// Extends the argument list of a [`MetaFunc`] to contain yet another generic argument.
    ///
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    ProvideGeneric(FuncExpr<'gcx>, FuncExpr<'gcx>),

    /// Instantiates a [`MetaFunc`], transforming it into a [`Func`].
    ///
    /// [`Func`]: super::TyKind::Func
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    Instantiate(FuncExpr<'gcx>, Ty<'gcx>),

    /// Produces a [`MetaFunc`] from the specified [`Func`] literal
    ///
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    FuncLiteral(Func<'gcx>),

    /// Produces a [`MetaType`] from the specified type.
    ///
    /// [`MetaType`]: super::TyKind::MetaType
    TypeOf(Ty<'gcx>),

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
    /// [`Func`]: super::TyKind::Func
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
    /// [`Func`]: super::TyKind::Func
    /// [`MetaFunc`]: super::TyKind::MetaFunc
    /// [`MetaType`]: super::TyKind::MetaType
    TypeWithStatic {
        target: FuncExpr<'gcx>,
        name: FuncExpr<'gcx>,
        producer: FuncExpr<'gcx>,
    },
}
