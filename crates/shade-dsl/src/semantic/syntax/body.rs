use crate::base::Symbol;

use super::Ty;

pub struct Func<'gcx> {
    pub arguments: &'gcx [FuncArg<'gcx>],
    pub ret_type: Ty<'gcx>,
    pub main: FuncBlock<'gcx>,
}

pub struct FuncArg<'gcx> {
    pub is_comptime: bool,
    pub name: Symbol,
    pub ty: Ty<'gcx>,
}

pub struct FuncBlock<'gcx> {}
