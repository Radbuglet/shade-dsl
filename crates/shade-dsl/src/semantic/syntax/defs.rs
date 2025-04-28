use crate::base::{Def, Gcx, Intern, InternList, Symbol};

use super::{Ty, Value, ValueList};

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

impl<'gcx> Instance<'gcx> {
    pub fn fully_specified(self) -> bool {
        self.func.generics.len() == self.generics.len()
    }

    pub fn as_bound(self, gcx: Gcx<'gcx>) -> BoundInstance<'gcx> {
        BoundInstance {
            func: self.func,
            generics: gcx
                .bound_value_list_interner
                .intern_iter(gcx, self.generics.iter().copied().map(BoundValue::Value)),
        }
    }
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

impl<'gcx> BoundInstance<'gcx> {
    pub fn fully_specified(self) -> bool {
        self.func.generics.len() == self.generics.len()
    }

    pub fn expect_unbound(self, gcx: Gcx<'gcx>) -> Instance<'gcx> {
        Instance {
            func: self.func,
            generics: gcx.value_list_interner.intern_iter(
                gcx,
                self.generics.iter().map(|&value| match value {
                    BoundValue::Value(value) => value,
                    BoundValue::Bound(_) => panic!("unexpected bound value"),
                }),
            ),
        }
    }

    pub fn resolve_in(self, gcx: Gcx<'gcx>, context: BoundInstance<'gcx>) -> BoundInstance<'gcx> {
        assert!(context.fully_specified());

        BoundInstance {
            func: self.func,
            generics: gcx.bound_value_list_interner.intern_iter(
                gcx,
                self.generics.iter().map(|&value| match value {
                    value @ BoundValue::Value(_) => value,
                    BoundValue::Bound(def) => {
                        let parent_idx = context
                            .func
                            .generics
                            .iter()
                            .position(|&other| def == other)
                            .expect("bound generic does not come from parent");

                        context.generics[parent_idx]
                    }
                }),
            ),
        }
    }
}

// === Functions === //

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
