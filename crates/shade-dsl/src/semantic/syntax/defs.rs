use crate::base::{Def, Intern, InternList, Symbol};

use super::{UnresolvedTy, Value, ValueList};

// === Adts === //

pub type ItemList<'gcx> = Intern<'gcx, Item<'gcx>>;

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
    pub ret_type: UnresolvedTy<'gcx>,
    pub main: FuncBlock<'gcx>,
}

pub type FuncGeneric<'gcx> = Def<'gcx, FuncGenericInner<'gcx>>;

pub struct FuncGenericInner<'gcx> {
    pub name: Symbol,
    pub ty: UnresolvedTy<'gcx>,
}

pub type FuncLocal<'gcx> = Def<'gcx, FuncLocalInner<'gcx>>;

pub struct FuncLocalInner<'gcx> {
    pub name: Symbol,
    pub ty: UnresolvedTy<'gcx>,
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
