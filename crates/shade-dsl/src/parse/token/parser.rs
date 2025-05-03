use crate::{
    base::{CharCursor, CharParser, Diag, Gcx, Level, Parser, Span, SpanCharCursor},
    symbol,
};

use super::{GroupDelimiter, Punct, TokenStream, TokenTree, TokenTreeKind};

type P<'a, 'gcx, 'ch> = &'a mut CharParser<'gcx, 'ch>;
type C<'a, 'gcx, 'ch> = &'a mut CharCursor<'ch>;

pub fn tokenize(gcx: Gcx<'_>, span: Span) -> TokenStream {
    let text = gcx.source_map.span_text(span);
    let mut parser = Parser::new(gcx, SpanCharCursor::new(span, &text));

    parser.context(symbol!("tokenizing the file"), |p| {
        parse_group(p, GroupDelimiter::File)
    })
}

fn parse_group(p: P, delimiter: GroupDelimiter) -> TokenStream {
    let mut stream = TokenStream::new();

    'parse: loop {
        let first_char = p.span();

        // Parse closing group delimiters
        if let Some(closing_del) = p.expect(delimiter.closing_name(), |c| {
            GroupDelimiter::CLOSEABLE
                .iter()
                .copied()
                .find(|v| match_ch(c, v.closing()))
        }) {
            if closing_del != delimiter {
                p.dcx().emit(
                    Diag::new(
                        Level::Error,
                        format_args!(
                            "mismatched delimiters; expected `{}`, got `{}`",
                            delimiter.closing(),
                            closing_del.closing()
                        ),
                    )
                    .primary(first_char, format!("expected `{}`", delimiter.closing())),
                );
            }

            break;
        }

        // Parse opening group delimiters
        for open_del in GroupDelimiter::OPENABLE {
            if p.expect(open_del.opening_name(), |c| match_ch(c, open_del.opening())) {
                let sub_stream = parse_group(p, open_del);

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
        let err = p.stuck();
        let c = p.recover(err);

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
