use crate::{
    base::{Def, Gcx, LeafDiag, Level, Matcher, OptPResult, OptPResultExt},
    parse::token::{
        GroupDelimiter, Ident, TokenCursor, TokenGroup, TokenMatcher, TokenParser, token_matcher,
    },
    symbol,
};

use super::{AstAdt, AstAdtInner, Keyword, kw};

type P<'gcx, 'a, 'g> = &'a mut TokenParser<'gcx, 'g>;
type C<'a, 'g> = &'a mut TokenCursor<'g>;

pub fn parse_file<'gcx>(gcx: Gcx<'gcx>, tokens: &TokenGroup) -> OptPResult<AstAdt<'gcx>> {
    let mut p = TokenParser::new(gcx, tokens);

    parse_mod_inner(&mut p)
}

fn parse_mod_inner<'gcx>(p: P<'gcx, '_, '_>) -> OptPResult<AstAdt<'gcx>> {
    let mut fields = Vec::new();
    let mut members = Vec::new();

    loop {
        if match_eos(p) {
            break;
        }

        if let Some(module) = parse_mod(p).did_match() {
            // TODO: push
            continue;
        }

        p.stuck_recover().eat();
    }

    Ok(Some(Def::new_alloc(
        p.gcx(),
        AstAdtInner {
            fields: p.gcx().alloc_slice(&fields),
            members: p.gcx().alloc_slice(&members),
        },
    )))
}

fn parse_mod<'gcx>(p: P<'gcx, '_, '_>) -> OptPResult<AstAdt<'gcx>> {
    // Match `mod`
    if match_kw(kw!("mod")).expect(p).is_none() {
        return Ok(None);
    }

    // Match module name
    let Some(name) = match_ident().expect(p) else {
        return Err(p.stuck());
    };

    // Match group
    let Some(group) = match_group(GroupDelimiter::Brace).expect(p) else {
        return Err(p.stuck());
    };

    parse_mod_inner(&mut p.enter(&group))
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
