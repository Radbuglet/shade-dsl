use unicode_xid::UnicodeXID;

use crate::{
    base::{CharCursor, CharParser, Diag, Gcx, Level, Parser, Span, SpanCharCursor, Symbol},
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
                .find(|v| c.lookahead(|c| c.eat() == v.closing()))
        }) {
            if closing_del != delimiter {
                p.dcx().emit(
                    Diag::new(
                        Level::Error,
                        format_args!(
                            "mismatched delimiters; expected `{}`, got `{}`",
                            delimiter.closing_name(),
                            closing_del.closing_name()
                        ),
                    )
                    .primary(
                        first_char,
                        format!("expected `{}`", delimiter.closing_name()),
                    ),
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
        if let Some((text, is_raw)) = parse_ident(p) {
            stream.push(TokenTree {
                span: first_char.until(p.span()),
                kind: TokenTreeKind::Ident(text, is_raw),
            });

            continue;
        }

        // Parse punctuation
        if let Some(ch) = p.expect(symbol!("punctuation"), |c| match_chs(c, Punct::CHARSET)) {
            stream.push(TokenTree {
                span: first_char,
                kind: TokenTreeKind::Punct(Punct::new(ch)),
            });

            continue;
        }

        // Parse whitespace.
        if p.expect(symbol!("whitespace"), |c| {
            c.eat().is_some_and(|c| c.is_whitespace())
        }) {
            continue;
        }

        // We're stuck :(
        let err = p.stuck();
        let c = p.recover(err);

        c.eat();
    }

    stream
}

fn parse_ident(p: P) -> Option<(Symbol, bool)> {
    let first_ch = p.expect(symbol!("identifier"), |c| {
        c.eat().filter(|c| c.is_xid_start())
    })?;

    let mut accum = String::new();

    let is_raw = if first_ch == 'r' && p.expect(symbol!("#"), |c| match_ch(c, '#')) {
        true
    } else {
        accum.push(first_ch);
        false
    };

    while let Some(ch) = p.expect(symbol!("identifier character"), |c| {
        c.eat().filter(|c| c.is_xid_continue())
    }) {
        accum.push(ch);
    }

    Some((Symbol::new(&accum), is_raw))
}

fn match_ch(c: C, ch: char) -> bool {
    c.lookahead(|c| c.eat() == Some(ch))
}

fn match_chs(c: C, set: &str) -> Option<char> {
    c.lookahead(|c| c.eat().filter(|&v| set.contains(v)))
}
