use crate::base::Symbol;

use super::{Ty, TyAdtItem, Value};

pub struct FuncLocal<'gcx> {
    pub is_comptime: bool,
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

pub struct Func<'gcx> {
    pub arguments: &'gcx [FuncLocal<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

pub struct FuncBlock<'gcx> {
    _ty: [&'gcx (); 0],
}

pub enum FuncStmt<'gcx> {
    Let(FuncLocal<'gcx>, &'gcx FuncExpr<'gcx>),
    Expr(&'gcx FuncExpr<'gcx>),
    Item(TyAdtItem<'gcx>),
}

pub enum FuncExpr<'gcx> {
    Call {
        receiver: &'gcx FuncExpr<'gcx>,
        args: &'gcx [FuncExpr<'gcx>],
    },
    Const {
        other: Value<'gcx>,
    },
}
