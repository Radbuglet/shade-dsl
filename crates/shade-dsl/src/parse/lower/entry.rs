use crate::{
    base::{W, mem::Component},
    parse::ast::{AstAdt, AstAdtKind, AstExpr},
    symbol,
    typeck::syntax::{AdtKind, AdtSignature, ObjAdtSignature, ValueInstance},
};

pub fn lower_adt(ast: &AstAdt, w: W) -> ObjAdtSignature {
    AdtSignature {
        span: ast.span,
        name: symbol!("file"), // TODO
        kind: match ast.kind {
            AstAdtKind::Mod(_) => AdtKind::Mod,
            AstAdtKind::Struct(_) => AdtKind::Struct,
            AstAdtKind::Enum(_) => AdtKind::Enum,
            AstAdtKind::Union(_) => AdtKind::Union,
        },
        fields: Vec::new(),
        members: Vec::new(),
    }
    .spawn(w)
}

pub fn lower_instance(expr: &AstExpr, w: W) -> ValueInstance {
    todo!()
}
