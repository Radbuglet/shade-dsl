use crate::{
    base::{Gcx, LeafDiag, Level, Matcher, OptPResult, OptPResultExt},
    parse::token::{
        GroupDelimiter, Ident, Punct, TokenCursor, TokenGroup, TokenMatcher, TokenParser,
        TokenPunct, token_matcher,
    },
    punct, symbol,
};

use super::{AdtKind, AstAdt, AstExpr, AstField, AstMember, AstMemberInit, AstType, Keyword, kw};

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

        // Parse `let <name>: <ty>;` fields.
        if let Some(field) = parse_adt_field(p).did_match() {
            if let Ok(field) = field {
                fields.push(field);
            }

            // recovery strategy: continue parsing where we left off.

            continue;
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
    // Match `let`
    if match_kw(kw!("let")).expect(p).is_none() {
        return Ok(None);
    }

    // Match name
    let Some(name) = match_ident().expect(p) else {
        return Err(p.stuck());
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

pub fn parse_expr(p: P) -> AstExpr {
    let start = p.span();

    // TODO
    p.expect(symbol!("expression"), |c| c.eat());

    AstExpr {
        span: start.until(p.span()),
    }
}

// === Type parsing === //

pub fn parse_ty(p: P) -> AstType {
    let start = p.span();

    // TODO
    p.expect(symbol!("type"), |c| c.eat());

    AstType {
        span: start.until(p.span()),
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
    token_matcher(kw.symbol(), move |c, _h| {
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
