use crate::base::Symbol;

use super::{Ty, TyAdtItem, Value};

#[derive(Clone)]
pub struct FuncLocal<'gcx> {
    pub is_comptime: bool,
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

#[derive(Clone)]
pub struct Func<'gcx> {
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

#[derive(Clone)]
pub struct FuncBlock<'gcx> {
    _ty: [&'gcx (); 0],
}

#[derive(Clone)]
pub enum FuncStmt<'gcx> {
    Let(FuncLocal<'gcx>, &'gcx FuncExpr<'gcx>),
    Expr(&'gcx FuncExpr<'gcx>),
    Item(TyAdtItem<'gcx>),
}

#[derive(Clone)]
pub enum FuncExpr<'gcx> {
    Call(&'gcx FuncExpr<'gcx>, &'gcx [FuncExpr<'gcx>]),
    Const(Value<'gcx>),
}
