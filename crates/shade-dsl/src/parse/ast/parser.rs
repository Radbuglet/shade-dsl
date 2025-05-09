use crate::{
    parse::token::{Ident, TokenCursor, TokenMatcher, TokenParser, token_matcher},
    symbol,
};

use super::Keyword;

type P<'a, 'gcx, 'g> = &'a mut TokenParser<'gcx, 'g>;
type C<'a, 'gcx, 'g> = &'a mut TokenCursor<'g>;

pub fn parse_module(p: P) {
    todo!()
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
    token_matcher(symbol!("identifier"), move |c, _h| {
        match_any_ident(c).filter(|v| v.raw || v.text.as_str(|s| Keyword::try_new(s).is_some()))
    })
}
