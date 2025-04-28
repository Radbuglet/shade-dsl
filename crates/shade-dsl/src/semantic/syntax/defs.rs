use crate::base::{Def, Intern, Symbol};

use super::{Ty, Value, ValueList};

// === Adts === //

pub type ItemList<'gcx> = Intern<'gcx, Item<'gcx>>;

pub type Item<'gcx> = Def<'gcx, ItemInner<'gcx>>;

pub struct ItemInner<'gcx> {
    pub name: Symbol,
    pub init: Instance<'gcx>,
}

// === Functions === //

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Instance<'gcx> {
    pub func: Func<'gcx>,
    pub generics: ValueList<'gcx>,
}

pub type Func<'gcx> = Def<'gcx, FuncInner<'gcx>>;

pub struct FuncInner<'gcx> {
    pub generics: &'gcx [FuncGeneric<'gcx>],
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

pub type FuncGeneric<'gcx> = Def<'gcx, FuncGenericInner<'gcx>>;

pub struct FuncGenericInner<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

pub type FuncLocal<'gcx> = Def<'gcx, FuncLocalInner<'gcx>>;

pub struct FuncLocalInner<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

pub struct FuncBlock<'gcx> {
    _ty: [&'gcx (); 0],
}

pub enum FuncStmt<'gcx> {
    Let(FuncLocal<'gcx>, FuncExpr<'gcx>),
    Expr(FuncExpr<'gcx>),
    Item(Item<'gcx>),
}

pub type FuncExpr<'gcx> = Def<'gcx, FuncExprInner<'gcx>>;

pub enum FuncExprInner<'gcx> {
    Local(FuncLocal<'gcx>),
    Generic(FuncGeneric<'gcx>),
    Item(Item<'gcx>),
    Call(FuncExpr<'gcx>, &'gcx [FuncExpr<'gcx>]),
    Const(Value<'gcx>),
}
