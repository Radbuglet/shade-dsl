use ctx2d_utils::hash::FxHashMap;
use derive_where::derive_where;

use crate::{
    base::syntax::Symbol,
    typeck::syntax::{ObjGenericDef, ObjLocalDef, UnevalInstance},
};

// === Resolver === //

#[derive(Debug, Clone)]
pub struct Resolver {
    pub names: SingleResolver<Name>,
    pub curr_depth: ExprDepth,
}

#[derive(Debug, Clone)]
pub enum Name {
    Generic(ObjGenericDef, ExprDepth),
    Local(ObjLocalDef, ExprDepth),
    Const(UnevalInstance, ExprDepth),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ExprDepth(pub u32);

// === SingleResolver === //

#[derive(Debug, Clone)]
#[derive_where(Default)]
pub struct SingleResolver<T> {
    map: FxHashMap<Symbol, T>,
    stack: Vec<Op<T>>,
}

#[derive(Debug, Clone)]
enum Op<T> {
    Set(Symbol, Option<T>),
    Rib,
}

impl<T> SingleResolver<T> {
    pub fn define(&mut self, sym: Symbol, value: T) {
        self.stack.push(Op::Set(sym, self.map.insert(sym, value)));
    }

    pub fn push_rib(&mut self) {
        self.stack.push(Op::Rib);
    }

    pub fn pop_rib(&mut self) {
        while let Some(op) = self.stack.pop() {
            let Op::Set(sym, prev) = op else {
                // Hit the previous `push`.
                break;
            };

            if let Some(prev) = prev {
                self.map.insert(sym, prev);
            } else {
                self.map.remove(&sym);
            }
        }
    }
}
