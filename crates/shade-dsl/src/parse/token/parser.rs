use crate::{
    base::{CharCursor, CharParser},
    symbol,
};

use super::{GroupDelimiter, Punct, TokenStream, TokenTree, TokenTreeKind};

type P<'a, 'b> = &'a mut CharParser<'b>;
type C<'a, 'b> = &'a mut CharCursor<'b>;

fn parse_group(p: P, del: GroupDelimiter) -> TokenStream {
    let mut stream = TokenStream::new();

    'parse: loop {
        let first_char = p.span();

        // Parse closing group delimiters
        if p.expect(del.closing_name(), |c| match_ch(c, del.closing())) {
            break;
        }

        // Parse opening group delimiters
        for del in GroupDelimiter::OPENABLE {
            if p.expect(del.opening_name(), |c| match_ch(c, del.opening())) {
                let sub_stream = parse_group(p, del);
                stream.push(TokenTree {
                    span: first_char.until(p.span()),
                    kind: TokenTreeKind::Group(del, sub_stream),
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

        for del in GroupDelimiter::CLOSEABLE {
            if c.peek() == del.closing() {
                // TODO
            }
        }

        c.eat();
    }

    stream
}

fn match_ch(c: C, ch: char) -> bool {
    c.lookahead(|c| c.eat() == ch)
}

fn match_chs(c: C, set: &str) -> Option<char> {
    c.lookahead(|c| set.contains(c.peek()).then_some(c.eat()))
}
