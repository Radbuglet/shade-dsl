use crate::base::{Id, Symbol};

use super::{Ty, TyAdtItem};

#[derive(Debug, Clone)]
pub struct FuncLocal<'gcx> {
    pub is_comptime: bool,
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

#[derive(Debug, Clone)]
pub struct Func<'gcx> {
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

#[derive(Debug, Clone)]
pub struct FuncBlock<'gcx> {
    _ty: [&'gcx (); 0],
}

#[derive(Debug, Clone)]
pub enum FuncStmt<'gcx> {
    Let(FuncLocal<'gcx>, Id<'gcx, FuncExpr<'gcx>>),
    Expr(Id<'gcx, FuncExpr<'gcx>>),
    Item(TyAdtItem<'gcx>),
}

#[derive(Debug, Clone)]
pub enum FuncExpr<'gcx> {
    Local(Id<'gcx, FuncLocal<'gcx>>),
    Item(Id<'gcx, TyAdtItem<'gcx>>),
    Call(Id<'gcx, FuncExpr<'gcx>>, &'gcx [FuncExpr<'gcx>]),
}
