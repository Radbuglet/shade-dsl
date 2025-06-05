use ctx2d_utils::hash::FxHashMap;
use derive_where::derive_where;

use crate::base::syntax::Symbol;

#[derive(Debug, Clone)]
#[derive_where(Default)]
pub struct NameResolver<T> {
    map: FxHashMap<Symbol, T>,
    stack: Vec<Op<T>>,
}

#[derive(Debug, Clone)]
enum Op<T> {
    Set(Symbol, Option<T>),
    Rib,
}

impl<T> NameResolver<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, sym: Symbol, value: T) {
        self.stack.push(Op::Set(sym, self.map.insert(sym, value)));
    }

    pub fn lookup(&self, sym: Symbol) -> Option<&T> {
        self.map.get(&sym)
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
