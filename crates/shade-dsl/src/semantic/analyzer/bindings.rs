use crate::{
    base::Gcx,
    semantic::syntax::{BoundInstance, BoundValue, Instance},
};

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
