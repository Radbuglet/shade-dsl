use crate::{base::Gcx, parse::ast::AstAdt};

pub struct AstLowerCtx<'gcx> {
    gcx: Gcx<'gcx>,
}

impl<'gcx> AstLowerCtx<'gcx> {
    pub fn collect(&mut self, root: &AstAdt) {}
}
