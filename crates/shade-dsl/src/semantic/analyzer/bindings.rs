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

    pub fn as_unbound(self, gcx: Gcx<'gcx>) -> Option<Instance<'gcx>> {
        let mut generics = Vec::with_capacity(self.generics.len());

        for &value in self.generics.iter() {
            let BoundValue::Value(value) = value else {
                return None;
            };

            generics.push(value);
        }

        Some(Instance {
            func: self.func,
            generics: gcx.value_list_interner.intern(gcx, &generics),
        })
    }

    pub fn expect_unbound(self, gcx: Gcx<'gcx>) -> Instance<'gcx> {
        self.as_unbound(gcx).expect("unexpected bound value")
    }

    pub fn resolve(self, gcx: Gcx<'gcx>, context: BoundInstance<'gcx>) -> BoundInstance<'gcx> {
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

    pub fn resolve_unbound(self, gcx: Gcx<'gcx>, context: BoundInstance<'gcx>) -> Instance<'gcx> {
        self.resolve(gcx, context).expect_unbound(gcx)
    }
}
