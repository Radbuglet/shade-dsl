use crate::{
    base::Gcx,
    semantic::syntax::{BoundInstance, BoundValue, FuncGeneric, Instance, Ty, TyKind, ValueKind},
};

pub fn rebind_generic<'gcx>(
    context: BoundInstance<'gcx>,
    para: FuncGeneric<'gcx>,
) -> BoundValue<'gcx> {
    let parent_idx = context
        .func
        .generics
        .iter()
        .position(|&other| para == other)
        .expect("bound generic does not come from parent");

    let expected_ty = context.func.generics[parent_idx].ty;

    let value = context.generics[parent_idx];

    match value {
        BoundValue::Value(val) => {
            assert!(val.ty == expected_ty);
        }
        BoundValue::Bound(def) => {
            assert!(def.ty == expected_ty);
        }
    }

    value
}

pub fn rebind_type<'gcx>(gcx: Gcx<'gcx>, context: BoundInstance<'gcx>, ty: Ty<'gcx>) -> Ty<'gcx> {
    match **ty {
        TyKind::Const(instance) => gcx.intern(TyKind::Const(instance.rebind(gcx, context))),
        TyKind::Generic(para) => {
            assert!(matches!(**para.ty, TyKind::MetaType));

            match rebind_generic(context, para) {
                BoundValue::Value(value) => {
                    let ValueKind::MetaType(ty) = value.kind else {
                        unreachable!();
                    };

                    ty
                }
                BoundValue::Bound(def) => gcx.intern(TyKind::Generic(def)),
            }
        }
        TyKind::Func(args, retval) => {
            let args = gcx.intern_iter(args.iter().map(|&arg| rebind_type(gcx, context, arg)));

            let retval = rebind_type(gcx, context, retval);

            gcx.intern(TyKind::Func(args, retval))
        }
        TyKind::Tuple(elems) => gcx.intern(TyKind::Tuple(
            gcx.intern_iter(elems.iter().map(|&elem| rebind_type(gcx, context, elem))),
        )),
        TyKind::MetaType | TyKind::MetaFunc | TyKind::Scalar(_) | TyKind::Adt(_) => ty,
    }
}

impl<'gcx> Instance<'gcx> {
    pub fn fully_specified(self) -> bool {
        self.func.generics.len() == self.generics.len()
    }

    pub fn as_bound(self, gcx: Gcx<'gcx>) -> BoundInstance<'gcx> {
        BoundInstance {
            func: self.func,
            generics: gcx.intern_iter(self.generics.iter().copied().map(BoundValue::Value)),
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
            generics: gcx.intern_slice(&generics),
        })
    }

    pub fn expect_unbound(self, gcx: Gcx<'gcx>) -> Instance<'gcx> {
        self.as_unbound(gcx).expect("unexpected bound value")
    }

    pub fn rebind(self, gcx: Gcx<'gcx>, context: BoundInstance<'gcx>) -> BoundInstance<'gcx> {
        assert!(context.fully_specified());

        BoundInstance {
            func: self.func,
            generics: gcx.intern_iter(self.generics.iter().map(|&value| match value {
                value @ BoundValue::Value(_) => value,
                BoundValue::Bound(def) => rebind_generic(context, def),
            })),
        }
    }

    pub fn rebind_unbound(self, gcx: Gcx<'gcx>, context: BoundInstance<'gcx>) -> Instance<'gcx> {
        self.rebind(gcx, context).expect_unbound(gcx)
    }
}
