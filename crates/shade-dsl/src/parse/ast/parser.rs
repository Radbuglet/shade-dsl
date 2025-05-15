use crate::{
    base::{Bp, Gcx, InfixBp, LeafDiag, Level, Matcher, OptPResult, OptPResultExt, Span},
    parse::{
        ast::bp,
        token::{
            GroupDelimiter, Ident, Punct, TokenCharLit, TokenCursor, TokenGroup, TokenMatcher,
            TokenParser, TokenPunct, TokenStrLit, token_matcher,
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

fn parse_adt_field(p: P) -> OptPResult<AstField> {
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

fn parse_expr_pratt(p: P, min_bp: Bp) -> AstExpr {
    parse_expr_pratt_inner(p, min_bp, false).unwrap()
}

fn parse_expr_pratt_opt(p: P, min_bp: Bp) -> Option<AstExpr> {
    parse_expr_pratt_inner(p, min_bp, true)
}

// See: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn parse_expr_pratt_inner(p: P, min_bp: Bp, is_optional: bool) -> Option<AstExpr> {
    // Parse seed expression
    let seed_start = p.next_span();
    let build_expr = move |expr: AstExprKind, p: P| AstExpr {
        span: seed_start.to(p.prev_span()),
        kind: expr,
    };

    let mut lhs = 'seed: {
        // Parse a name.
        if let Some(ident) = match_ident().expect(p) {
            break 'seed build_expr(AstExprKind::Name(ident), p);
        }

        // Parse a boolean literal.
        if match_kw(kw!("true")).expect(p).is_some() {
            break 'seed build_expr(AstExprKind::BoolLit(true), p);
        }

        if match_kw(kw!("false")).expect(p).is_some() {
            break 'seed build_expr(AstExprKind::BoolLit(false), p);
        }

        // Parse a string literal.
        if let Some(lit) = match_str_lit().expect(p) {
            break 'seed build_expr(AstExprKind::StrLit(lit), p);
        }

        // Parse a character literal.
        if let Some(lit) = match_char_lit().expect(p) {
            break 'seed build_expr(AstExprKind::CharLit(lit), p);
        }

        // Parse a parenthesis or tuple.
        if let Some(paren) = match_group(GroupDelimiter::Paren).expect(p) {
            let mut p2 = p.enter(&paren);
            let mut had_comma = false;
            let mut exprs = Vec::new();

            loop {
                if match_eos(&mut p2) {
                    break;
                }

                exprs.push(parse_expr(&mut p2));

                if match_punct(punct!(',')).expect(&mut p2).is_some() {
                    had_comma = true;
                    continue;
                }

                break;
            }

            if had_comma || exprs.len() != 1 {
                break 'seed build_expr(AstExprKind::Tuple(exprs), p);
            } else {
                break 'seed build_expr(AstExprKind::Paren(Box::new(exprs.pop().unwrap())), p);
            }
        }

        // Parse a block expression
        if let Some(block) = parse_brace_block(p) {
            // TODO: Labels
            break 'seed build_expr(AstExprKind::Block(Box::new(block)), p);
        }

        // Parse an `if` expression
        if match_kw(kw!("if")).expect(p).is_some() {
            fn parse_after_if(if_span: Span, p: P) -> AstExpr {
                let cond = parse_expr(p);

                let Some(truthy) = parse_brace_block(p) else {
                    // Recovery strategy: do nothing
                    return AstExpr {
                        span: if_span.to(p.next_span()),
                        kind: AstExprKind::Error(p.stuck().0),
                    };
                };

                let self_span = if_span.to(p.prev_span());

                // Match `else`
                let Some(_) = match_kw(kw!("else")).expect(p) else {
                    // No `if` branch
                    return AstExpr {
                        span: self_span,
                        kind: AstExprKind::If {
                            cond: Box::new(cond),
                            truthy: Box::new(truthy),
                            falsy: None,
                        },
                    };
                };

                // Match `if`
                let else_if_start = p.next_span();
                let falsy = if match_kw(kw!("if")).expect(p).is_some() {
                    let falsy = parse_after_if(else_if_start, p);

                    AstBlock {
                        label: None,
                        span: falsy.span,
                        stmts: vec![],
                        last_expr: Some(falsy),
                    }
                } else {
                    // Match bare `else`
                    let Some(falsy) = parse_brace_block(p) else {
                        // Recovery strategy: do nothing
                        return AstExpr {
                            span: if_span.to(p.next_span()),
                            kind: AstExprKind::Error(p.stuck().0),
                        };
                    };

                    falsy
                };

                AstExpr {
                    span: self_span,
                    kind: AstExprKind::If {
                        cond: Box::new(cond),
                        truthy: Box::new(truthy),
                        falsy: Some(Box::new(falsy)),
                    },
                }
            }

            break 'seed parse_after_if(seed_start, p);
        }

        // Parse a `while` expression
        if match_kw(kw!("while")).expect(p).is_some() {
            // TODO: Labels

            let cond = parse_expr(p);

            let Some(block) = parse_brace_block(p) else {
                // Recovery strategy: do nothing
                break 'seed build_expr(AstExprKind::Error(p.stuck().0), p);
            };

            break 'seed build_expr(
                AstExprKind::While {
                    cond: Box::new(cond),
                    block: Box::new(block),
                },
                p,
            );
        }

        // Parse a `loop` expression
        if match_kw(kw!("loop")).expect(p).is_some() {
            // TODO: Labels

            let Some(block) = parse_brace_block(p) else {
                // Recovery strategy: do nothing
                break 'seed build_expr(AstExprKind::Error(p.stuck().0), p);
            };

            break 'seed build_expr(AstExprKind::Loop(Box::new(block)), p);
        }

        // Parse a `return` expression
        if match_kw(kw!("return")).expect(p).is_some() {
            let expr = parse_expr_pratt_opt(p, bp::PRE_RETURN.right);

            break 'seed build_expr(AstExprKind::Return(expr.map(Box::new)), p);
        }

        // Parse a `continue` expression
        if match_kw(kw!("continue")).expect(p).is_some() {
            break 'seed build_expr(AstExprKind::Continue, p);
        }

        // Parse a `break` expression
        if match_kw(kw!("break")).expect(p).is_some() {
            // TODO: Labels

            let expr = parse_expr_pratt_opt(p, bp::PRE_BREAK.right);

            break 'seed build_expr(AstExprKind::Break(expr.map(Box::new)), p);
        }

        // Parse a `return` expression
        if match_kw(kw!("return")).expect(p).is_some() {
            let expr = parse_expr_pratt_opt(p, bp::PRE_RETURN.right);

            break 'seed build_expr(AstExprKind::Return(expr.map(Box::new)), p);
        }

        // Parse unary neg.
        if match_punct(punct!('-')).expect(p).is_some() {
            let lhs = parse_expr_pratt(p, bp::PRE_NEG.right);

            break 'seed build_expr(AstExprKind::UnaryNeg(Box::new(lhs)), p);
        }

        // Parse unary not.
        if match_punct(punct!('!')).expect(p).is_some() {
            let lhs = parse_expr_pratt(p, bp::PRE_NOT.right);

            break 'seed build_expr(AstExprKind::UnaryNot(Box::new(lhs)), p);
        }

        if is_optional {
            return None;
        } else {
            // Recovery strategy: eat a token
            build_expr(
                AstExprKind::Error(p.stuck_recover_with(|c| {
                    c.eat();
                })),
                p,
            )
        }
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

        // Match calls
        if let Some(group) =
            match_group(GroupDelimiter::Paren).maybe_expect(p, bp::POST_CALL.left >= min_bp)
        {
            let mut args = Vec::new();
            let mut p = p.enter(&group);

            loop {
                if match_eos(&mut p) {
                    break;
                }

                let expr = parse_expr(&mut p);
                args.push(expr);

                if match_punct(punct!(',')).expect(&mut p).is_none() {
                    if match_eos(&mut p) {
                        break;
                    }

                    // Recovery strategy: ignore remainder.
                    let _ = p.stuck();
                    break;
                }
            }

            lhs = AstExpr {
                span: group.span,
                kind: AstExprKind::Call(Box::new(lhs), args),
            };

            continue;
        }

        // Match instantiations and named indexes
        if let Some(dot) = match_punct(punct!('.')).maybe_expect(p, bp::POST_DOT.left >= min_bp) {
            if match_punct(punct!('<')).expect(p).is_some() {
                todo!()
            } else {
                let Some(name) = match_ident().expect(p) else {
                    // Recovery strategy: keep trying to chain.
                    let _ = p.stuck();
                    continue 'chaining;
                };

                lhs = AstExpr {
                    span: dot.span.to(name.span),
                    kind: AstExprKind::NamedIndex(Box::new(lhs), name),
                };
            }

            continue 'chaining;
        }

        // Match infix assignment
        if let Some(punct) =
            match_punct(punct!('=')).maybe_expect(p, bp::INFIX_ASSIGN.left >= min_bp)
        {
            lhs = AstExpr {
                span: punct.span,
                kind: AstExprKind::Assign(
                    Box::new(lhs),
                    Box::new(parse_expr_pratt(p, bp::INFIX_ASSIGN.right)),
                ),
            };
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

    Some(lhs)
}

fn parse_brace_block(p: P) -> Option<AstBlock> {
    match_group(GroupDelimiter::Brace)
        .expect(p)
        .map(|group| parse_block(&mut p.enter(&group), None))
}

fn parse_block(p: P, label: Option<Ident>) -> AstBlock {
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

fn parse_ty(p: P) -> AstType {
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

fn match_str_lit() -> impl TokenMatcher<Output = Option<TokenStrLit>> {
    token_matcher(symbol!("string literal"), |c, _| {
        c.eat().and_then(|v| v.str_lit()).copied()
    })
}

fn match_char_lit() -> impl TokenMatcher<Output = Option<TokenCharLit>> {
    token_matcher(symbol!("string literal"), |c, _| {
        c.eat().and_then(|v| v.char_lit()).copied()
    })
}
