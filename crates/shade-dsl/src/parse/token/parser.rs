use crate::{
    base::{CharCursor, CharParser, Symbol},
    symbol,
};

use super::{GroupDelimiter, Punct, TokenStream, TokenTree, TokenTreeKind};

type P<'a, 'b> = &'a mut CharParser<'b>;
type C<'a, 'b> = &'a mut CharCursor<'b>;

fn parse_group(p: P, expected_closer_name: Symbol) -> (TokenStream, GroupDelimiter) {
    let mut stream = TokenStream::new();

    let closing_del = 'parse: loop {
        let first_char = p.span();

        // Parse closing group delimiters
        if let Some(closing_del) = p.expect(expected_closer_name, |c| {
            GroupDelimiter::CLOSEABLE
                .iter()
                .copied()
                .find(|v| match_ch(c, v.closing()))
        }) {
            break closing_del;
        }

        // Parse opening group delimiters
        for open_del in GroupDelimiter::OPENABLE {
            if p.expect(open_del.opening_name(), |c| match_ch(c, open_del.opening())) {
                let (sub_stream, close_del) = parse_group(p, open_del.opening_name());

                if open_del != close_del {
                    // TODO: handle `found_del` mismatches
                }

                stream.push(TokenTree {
                    span: first_char.until(p.span()),
                    kind: TokenTreeKind::Group(open_del, sub_stream),
                });

                continue 'parse;
            }
        }

        // Parse comments
        // TODO

        // Parse string literals
        // TODO

        // Parse numeric literals
        // TODO

        // Parse identifiers
        // TODO

        // Parse punctuation
        if let Some(ch) = p.expect(symbol!("punctuation"), |c| match_chs(c, Punct::CHARSET)) {
            stream.push(TokenTree {
                span: first_char,
                kind: TokenTreeKind::Punct(Punct::new(ch)),
            });

            continue;
        }

        // Parse whitespace.
        if p.expect(symbol!("whitespace"), |c| c.eat().is_whitespace()) {
            continue;
        }

        // We're stuck :(
        let c = p.recover(p.stuck());

        c.eat();
    };

    (stream, closing_del)
}

fn match_ch(c: C, ch: char) -> bool {
    c.lookahead(|c| c.eat() == ch)
}

fn match_chs(c: C, set: &str) -> Option<char> {
    c.lookahead(|c| set.contains(c.peek()).then_some(c.eat()))
}
