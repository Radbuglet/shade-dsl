use crate::{
    base::{Session, arena::Obj},
    typeck::syntax::{Func, FuncInstance},
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
