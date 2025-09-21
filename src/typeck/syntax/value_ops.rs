use crate::{
    base::{Session, arena::Obj},
    typeck::syntax::{Func, FuncInstance, ScalarKind, ValueScalar},
};

pub fn find_ancestor_for_func(
    descendant: Obj<FuncInstance>,
    func: Obj<Func>,
    s: &Session,
) -> Obj<FuncInstance> {
    let mut iter = Some(descendant);

    while let Some(curr) = iter {
        if curr.r(s).func == func {
            return curr;
        }

        iter = curr.r(s).parent;
    }

    unreachable!()
}

impl ValueScalar {
    pub fn kind(&self) -> ScalarKind {
        match self {
            ValueScalar::Bool(_) => ScalarKind::Bool,
            ValueScalar::Char(_) => ScalarKind::Char,
            ValueScalar::U8(_) => ScalarKind::U8,
            ValueScalar::I8(_) => ScalarKind::I8,
            ValueScalar::U16(_) => ScalarKind::U16,
            ValueScalar::I16(_) => ScalarKind::I16,
            ValueScalar::U32(_) => ScalarKind::U32,
            ValueScalar::I32(_) => ScalarKind::I32,
            ValueScalar::U64(_) => ScalarKind::U64,
            ValueScalar::I64(_) => ScalarKind::I64,
            ValueScalar::U128(_) => ScalarKind::U128,
            ValueScalar::I128(_) => ScalarKind::I128,
            ValueScalar::F32(_) => ScalarKind::F32,
            ValueScalar::F64(_) => ScalarKind::F64,
            ValueScalar::USize(_) => ScalarKind::USize,
            ValueScalar::ISize(_) => ScalarKind::ISize,
        }
    }
}
