use std::{hash, ptr};

use crate::base::{Intern, Symbol};

use super::{Ty, Value, ValueList};

// === Adts === //

pub type ItemList<'gcx> = Intern<'gcx, Item<'gcx>>;

#[derive(Copy, Clone)]
pub struct Item<'gcx>(&'gcx ItemInner<'gcx>);

impl hash::Hash for Item<'_> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const ItemInner<'_>).hash(state);
    }
}

impl Eq for Item<'_> {}

impl PartialEq for Item<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}

pub struct ItemInner<'gcx> {
    pub name: Symbol,
    pub init: Instance<'gcx>,
}

// === Functions === //

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Instance<'gcx> {
    pub func: Func<'gcx>,
    pub provided_comptimes: ValueList<'gcx>,
}

#[derive(Copy, Clone)]
pub struct Func<'gcx>(&'gcx FuncInner<'gcx>);

impl hash::Hash for Func<'_> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const FuncInner<'_>).hash(state);
    }
}

impl Eq for Func<'_> {}

impl PartialEq for Func<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::addr_eq(self.0, other.0)
    }
}

#[derive(Clone)]
pub struct FuncInner<'gcx> {
    pub comptime_arguments: &'gcx [&'gcx FuncLocal<'gcx>],
    pub runtime_arguments: &'gcx [&'gcx FuncLocal<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

#[derive(Clone)]
pub struct FuncLocal<'gcx> {
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

#[derive(Clone)]
pub struct FuncBlock<'gcx> {
    _ty: [&'gcx (); 0],
}

#[derive(Clone)]
pub enum FuncStmt<'gcx> {
    Let(&'gcx FuncLocal<'gcx>, &'gcx FuncExpr<'gcx>),
    Expr(&'gcx FuncExpr<'gcx>),
    Item(Item<'gcx>),
}

#[derive(Clone)]
pub enum FuncExpr<'gcx> {
    Local(&'gcx FuncLocal<'gcx>),
    Item(&'gcx ItemInner<'gcx>),
    Call(&'gcx FuncExpr<'gcx>, &'gcx [FuncExpr<'gcx>]),
    Const(Value<'gcx>),
}
