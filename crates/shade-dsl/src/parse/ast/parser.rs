use crate::{
    base::{Bp, Gcx, InfixBp, LeafDiag, Level, Matcher, OptPResult, OptPResultExt},
    parse::{
        ast::bp,
        token::{
            GroupDelimiter, Ident, Punct, TokenCursor, TokenGroup, TokenMatcher, TokenParser,
            TokenPunct, token_matcher,
        },
    },
    punct, symbol,
};

use super::{
    AdtKind, AstAdt, AstBlock, AstExpr, AstExprKind, AstField, AstMember, AstMemberInit, AstStmt,
    AstStmtKind, AstType, Keyword, kw,
};

type P<'gcx, 'a, 'g> = &'a mut TokenParser<'gcx, 'g>;
type C<'a, 'g> = &'a mut TokenCursor<'g>;

// === ADT Parsing === //

pub fn parse_file(gcx: Gcx<'_>, tokens: &TokenGroup) -> AstAdt {
    let mut p = TokenParser::new(gcx, tokens);

    parse_adt_contents(&mut p, AdtKind::Mod(tokens.span.shrink_to_lo()))
}

fn parse_adt_contents(p: P, kind: AdtKind) -> AstAdt {
    let mut fields = Vec::new();
    let mut members = Vec::new();

    loop {
        if match_eos(p) {
            break;
        }

        // Parse `<mod|struct|enum|...> <name>` members.
        if let Some(member) = parse_adt_member_adt(p).did_match() {
            if let Ok(member) = member {
                members.push(member);
            }

            // recovery strategy: continue parsing where we left off.

            continue;
        }

        // Parse `const <name> = <expr>;` members.
        if let Some(member) = parse_ast_member_const(p).did_match() {
            if let Ok(member) = member {
                members.push(member);
            }

            // recovery strategy: continue parsing where we left off.

            continue;
        }

        // Parse `<name>: <ty>;` fields.
        if kind.can_have_fields() {
            if let Some(field) = parse_adt_field(p).did_match() {
                if let Ok(field) = field {
                    fields.push(field);
                }

                // recovery strategy: continue parsing where we left off.

                continue;
            }
        } else {
            let gcx = p.gcx();

            p.hint_if_passes(
                |c, _| {
                    if match_ident().consume(c).is_none() {
                        return false;
                    }

                    if match_punct(punct!(':')).consume(c).is_none() {
                        return false;
                    }

                    let start = c.prev_span();
                    let file = gcx.source_map.file(start.lo);
                    let start = file.pos_to_loc(start.lo).line;

                    c.lookahead(|c| {
                        loop {
                            if c.peek().is_none() {
                                return false;
                            }

                            if file.pos_to_loc(c.next_span().lo).line != start {
                                return false;
                            }

                            if match_punct(punct!(';')).consume(c).is_some() {
                                return true;
                            }

                            c.eat();
                        }
                    });

                    true
                },
                |sp, _| LeafDiag::span_note(sp, "modules cannot define fields"),
            );
        }

        p.stuck_recover().eat();
    }

    AstAdt {
        kind,
        fields,
        members,
    }
}

pub fn parse_adt_field(p: P) -> OptPResult<AstField> {
    // Match name
    let Some(name) = match_ident().expect(p) else {
        return Ok(None);
    };

    // Match annotation colon
    if match_punct(punct!(':')).expect(p).is_none() {
        return Err(p.stuck());
    }

    // Match type
    let ty = parse_ty(p);

    // Match semi-colon.
    if match_punct(punct!(';')).expect(p).is_none() {
        return Err(p.stuck());
    }

    Ok(Some(AstField { name, ty }))
}

fn parse_adt_member_adt(p: P) -> OptPResult<AstMember> {
    // Match `mod`
    let Some(kind) = parse_adt_kind(p) else {
        return Ok(None);
    };

    // Match module name
    let Some(name) = match_ident().expect(p) else {
        return Err(p.stuck());
    };

    // Match group
    let Some(group) = match_group(GroupDelimiter::Brace).expect(p) else {
        return Err(p.stuck());
    };

    // Match inner ADT
    let adt = parse_adt_contents(&mut p.enter(&group), kind);

    Ok(Some(AstMember {
        name,
        initializer: AstMemberInit::Adt(Box::new(adt)),
    }))
}

fn parse_ast_member_const(p: P) -> OptPResult<AstMember> {
    // Match `const`
    if match_kw(kw!("const")).expect(p).is_none() {
        return Ok(None);
    }

    // Match const name
    let Some(name) = match_ident().expect(p) else {
        return Err(p.stuck());
    };

    // Match equals symbol
    if match_punct(punct!('=')).expect(p).is_none() {
        return Err(p.stuck());
    }

    // Match initializer.
    let initializer = parse_expr(p);

    // Match semi-colon.
    if match_punct(punct!(';')).expect(p).is_none() {
        return Err(p.stuck());
    }

    Ok(Some(AstMember {
        name,
        initializer: AstMemberInit::Const(Box::new(initializer)),
    }))
}

// === Expression parsing === //

fn parse_expr(p: P) -> AstExpr {
    parse_expr_pratt(p, Bp::MIN)
}

fn parse_expr_full(p: P) -> AstExpr {
    let expr = parse_expr(p);

    if !match_eos(p) {
        // Recovery strategy: ignore
        let _ = p.stuck();
    }

    expr
}

// See: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn parse_expr_pratt(p: P, min_bp: Bp) -> AstExpr {
    // Parse seed expression
    let seed_start = p.next_span();
    let lhs = 'seed: {
        // Parse a name.
        if let Some(ident) = match_ident().expect(p) {
            break 'seed AstExprKind::Name(ident);
        }

        // Parse a boolean literal.
        if match_kw(kw!("true")).expect(p).is_some() {
            break 'seed AstExprKind::BoolLit(true);
        }

        if match_kw(kw!("false")).expect(p).is_some() {
            break 'seed AstExprKind::BoolLit(false);
        }

        // Parse a parenthesis or tuple.
        if let Some(paren) = match_group(GroupDelimiter::Paren).expect(p) {
            let mut p = p.enter(&paren);
            let mut had_comma = false;
            let mut exprs = Vec::new();

            loop {
                if match_eos(&mut p) {
                    break;
                }

                exprs.push(parse_expr(&mut p));

                if match_punct(punct!(',')).expect(&mut p).is_some() {
                    had_comma = true;
                    continue;
                }

                break;
            }

            if had_comma || exprs.len() != 1 {
                break 'seed AstExprKind::Tuple(exprs);
            } else {
                break 'seed AstExprKind::Paren(Box::new(exprs.pop().unwrap()));
            }
        }

        // Parse a block expression.
        if let Some(brace) = match_group(GroupDelimiter::Brace).expect(p) {
            break 'seed AstExprKind::Block(Box::new(parse_block(&mut p.enter(&brace), None)));
        }

        // Parse an `if` statement
        if match_kw(kw!("if")).expect(p).is_some() {
            let cond = parse_expr(p);

            let Some(truthy) = match_group(GroupDelimiter::Brace).expect(p) else {
                // Recovery strategy: do nothing
                break 'seed AstExprKind::Error(p.stuck().0);
            };

            let truthy = Box::new(parse_block(&mut p.enter(&truthy), None));

            let falsy = if match_kw(kw!("else")).expect(p).is_some() {
                let Some(falsy) = match_group(GroupDelimiter::Brace).expect(p) else {
                    // Recovery strategy: do nothing
                    break 'seed AstExprKind::Error(p.stuck().0);
                };

                Some(Box::new(parse_block(&mut p.enter(&falsy), None)))
            } else {
                None
            };

            break 'seed AstExprKind::If {
                cond: Box::new(cond),
                truthy,
                falsy,
            };
        }

        // Parse unary neg.
        if match_punct(punct!('-')).expect(p).is_some() {
            let lhs = parse_expr_pratt(p, bp::PRE_NEG.right);

            break 'seed AstExprKind::UnaryNeg(Box::new(lhs));
        }

        // Parse unary not.
        if match_punct(punct!('!')).expect(p).is_some() {
            let lhs = parse_expr_pratt(p, bp::PRE_NOT.right);

            break 'seed AstExprKind::UnaryNot(Box::new(lhs));
        }

        // Recovery strategy: do nothing
        AstExprKind::Error(p.stuck().0)
    };

    let mut lhs = AstExpr {
        span: seed_start.to(p.prev_span()),
        kind: lhs,
    };

    // Parse postfix and infix operations that bind tighter than our caller.
    'chaining: loop {
        // Match indexing
        if let Some(group) =
            match_group(GroupDelimiter::Bracket).maybe_expect(p, bp::POST_BRACKET.left >= min_bp)
        {
            let index = parse_expr_full(&mut p.enter(&group));

            lhs = AstExpr {
                span: group.span,
                kind: AstExprKind::Index(Box::new(lhs), Box::new(index)),
            };

            continue;
        }

        // Match punctuation-demarcated infix operations
        type PunctInfixOp = (
            Punct,
            InfixBp,
            fn(Box<AstExpr>, Box<AstExpr>) -> AstExprKind,
        );

        const PUNCT_INFIX_OPS: [PunctInfixOp; 5] = [
            (punct!('+'), bp::INFIX_ADD, AstExprKind::Add),
            (punct!('-'), bp::INFIX_SUB, AstExprKind::Sub),
            (punct!('*'), bp::INFIX_MUL, AstExprKind::Mul),
            (punct!('/'), bp::INFIX_DIV, AstExprKind::Div),
            (punct!('%'), bp::INFIX_MOD, AstExprKind::Mod),
        ];

        for (punct, op_bp, op_ctor) in PUNCT_INFIX_OPS {
            if let Some(op_punct) = match_punct(punct).maybe_expect(p, op_bp.left >= min_bp) {
                lhs = AstExpr {
                    span: op_punct.span,
                    kind: op_ctor(Box::new(lhs), Box::new(parse_expr_pratt(p, op_bp.right))),
                };

                continue 'chaining;
            }
        }

        break;
    }

    lhs
}

pub fn parse_block(p: P, label: Option<Ident>) -> AstBlock {
    let start = p.next_span();

    let mut stmts = Vec::new();

    let last_expr = loop {
        // Match EOS without unterminated expression.
        if match_eos(p) {
            break None;
        }

        // Match empty statements.
        if match_punct(punct!(';')).expect(p).is_some() {
            continue;
        }

        // Match let statements
        if match_kw(kw!("let")).expect(p).is_some() {
            todo!()
        }

        // Match expressions
        let expr = parse_expr(p);

        if match_eos(p) {
            break Some(expr);
        }

        let needs_semi = expr.kind.needs_semi();

        stmts.push(AstStmt {
            span: expr.span,
            kind: AstStmtKind::Expr(expr.kind),
        });

        if needs_semi && match_punct(punct!(';')).expect(p).is_none() {
            // Recovery strategy: ignore
            let _ = p.stuck_recover();
        }
    };

    AstBlock {
        label,
        span: start.to(p.prev_span()),
        stmts,
        last_expr,
    }
}

// === Type parsing === //

pub fn parse_ty(p: P) -> AstType {
    let start = p.next_span();

    // TODO
    p.expect(symbol!("type"), |c| c.eat());

    AstType {
        span: start.to(p.prev_span()),
    }
}

// === Parsing helpers === //

fn parse_adt_kind(p: P) -> Option<AdtKind> {
    if let Some(ident) = match_kw(kw!("mod")).expect(p) {
        return Some(AdtKind::Mod(ident.span));
    }

    if let Some(ident) = match_kw(kw!("struct")).expect(p) {
        return Some(AdtKind::Struct(ident.span));
    }

    if let Some(ident) = match_kw(kw!("enum")).expect(p) {
        return Some(AdtKind::Enum(ident.span));
    }

    if let Some(ident) = match_kw(kw!("union")).expect(p) {
        return Some(AdtKind::Union(ident.span));
    }

    None
}

fn match_any_ident(c: C) -> Option<Ident> {
    c.lookahead(|c| Some(*c.eat()?.ident()?))
}

fn match_kw(kw: Keyword) -> impl TokenMatcher<Output = Option<Ident>> {
    token_matcher(kw.expectation_name(), move |c, _h| {
        match_any_ident(c).filter(|v| !v.raw && v.text == kw.symbol())
    })
}

fn match_ident() -> impl TokenMatcher<Output = Option<Ident>> {
    token_matcher(symbol!("identifier"), move |c, h| {
        let ident = match_any_ident(c)?;

        if ident.raw {
            return Some(ident);
        }

        if ident.text.as_str(|s| Keyword::try_new(s).is_some()) {
            h.hint(LeafDiag::new(
                Level::Note,
                format_args!("`{}` is a reserved keyword", ident.text),
            ));

            return None;
        }

        Some(ident)
    })
}

fn match_group(group: GroupDelimiter) -> impl TokenMatcher<Output = Option<TokenGroup>> {
    token_matcher(group.opening_name(), move |c, _| {
        Some(c.eat()?.group().filter(|v| v.delimiter == group)?.clone())
    })
}

fn match_eos(p: P) -> bool {
    let closing_name = p.cursor_unsafe().raw.group().delimiter.closing_name();

    p.expect(closing_name, |v| v.eat().is_none())
}

fn match_punct(punct: Punct) -> impl TokenMatcher<Output = Option<TokenPunct>> {
    token_matcher(punct.expectation_name(), move |c, _| {
        c.eat()
            .and_then(|v| v.punct())
            .filter(|v| v.ch == punct)
            .copied()
    })
}
