use crate::{base::Def, parse::token::Ident};

pub type AstAdt<'gcx> = Def<'gcx, AstAdtInner<'gcx>>;

pub struct AstAdtInner<'gcx> {
    pub fields: &'gcx [AstField<'gcx>],
    pub members: &'gcx [()],
}

#[derive(Clone)]
pub struct AstField<'gcx> {
    pub name: Ident,
    pub ty: [&'gcx (); 0],
}
