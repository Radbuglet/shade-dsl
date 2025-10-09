use std::ops::ControlFlow;

use crate::{
    base::{Session, arena::Obj},
    typeck::syntax::{Local, Pat, PatKind},
};

pub fn visit_named_places<B>(
    pat: Obj<Pat>,
    s: &Session,
    mut f: impl FnMut(Obj<Local>) -> ControlFlow<B>,
) -> ControlFlow<B> {
    fn visit_named_places_inner<B>(
        pat: Obj<Pat>,
        s: &Session,
        f: &mut impl FnMut(Obj<Local>) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match pat.r(s).kind {
            PatKind::Hole => {
                // (no-op)
            }
            PatKind::Name(local) => {
                f(local)?;
            }
            PatKind::Tuple(ref children) => {
                for child in children {
                    visit_named_places_inner(*child, s, f)?;
                }
            }
            PatKind::Error(_) => {
                // (no-op)
            }
        }

        ControlFlow::Continue(())
    }

    visit_named_places_inner(pat, s, &mut f)
}
