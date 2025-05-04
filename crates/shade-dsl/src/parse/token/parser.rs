use unicode_xid::UnicodeXID;

use crate::{
    base::{CharCursor, CharParser, Diag, Gcx, Parser, Span, SpanCharCursor, Symbol},
    symbol,
};

use super::{GroupDelimiter, Ident, Punct, TokenGroup, TokenPunct, TokenStream, TokenTree};

type P<'a, 'gcx, 'ch> = &'a mut CharParser<'gcx, 'ch>;
type C<'a, 'gcx, 'ch> = &'a mut CharCursor<'ch>;

pub fn tokenize(gcx: Gcx<'_>, span: Span) -> TokenStream {
    let text = gcx.source_map.span_text(span);
    let mut parser = Parser::new(gcx, SpanCharCursor::new(span, &text));

    parser.context(symbol!("tokenizing the file"), |p| {
        parse_group(p, GroupDelimiter::File)
    })
}

#[derive(Default)]
struct GroupBuilder {
    stream: TokenStream,
    glued: bool,
}

impl GroupBuilder {
    fn push(&mut self, token: impl Into<TokenTree>) {
        self.glued = false;
        self.stream.push(token);
    }

    fn push_space(&mut self) {
        self.glued = false;
    }

    pub fn glued_punct(&self) -> Option<&TokenPunct> {
        self.stream
            .last()
            .and_then(|v| v.punct())
            .filter(|_| self.glued)
    }
}

fn parse_group(p: P, delimiter: GroupDelimiter) -> TokenStream {
    let mut builder = GroupBuilder::default();

    'parse: loop {
        let start_span = p.span();

        // Parse closing group delimiters
        if let Some(closing_del) = p.expect(delimiter.closing_name(), |c| {
            GroupDelimiter::CLOSEABLE
                .iter()
                .copied()
                .find(|v| c.lookahead(|c| c.eat() == v.closing()))
        }) {
            if closing_del != delimiter {
                p.dcx().emit(Diag::span_err(
                    start_span,
                    format_args!(
                        "{} delimiter; expected `{}`, got `{}`",
                        if closing_del == GroupDelimiter::File {
                            "unclosed"
                        } else {
                            "mismatched"
                        },
                        delimiter.closing_name(),
                        closing_del.closing_name()
                    ),
                ));
            }

            break;
        }

        // Parse opening group delimiters
        for open_del in GroupDelimiter::OPENABLE {
            if p.expect(open_del.opening_name(), |c| match_ch(c, open_del.opening())) {
                let sub_stream = parse_group(p, open_del);

                builder.push(TokenGroup {
                    span: start_span.until(p.span()),
                    delimiter,
                    tokens: sub_stream,
                });

                continue 'parse;
            }
        }

        // Parse comments
        if p.expect(symbol!("`//`"), |c| {
            c.eat() == Some('/') && c.eat() == Some('/')
        }) {
            while p.expect(symbol!("commented character"), |c| {
                c.eat().is_none_or(|v| v != '\n')
            }) {
                // (continue eating)
            }

            continue;
        }

        // Parse string literals
        // TODO

        // Parse numeric literals
        // TODO

        // Parse identifiers
        if let Some(ident) = parse_ident(p) {
            builder.push(ident);
            continue;
        }

        // Parse punctuation
        if let Some(ch) = p.expect(symbol!("punctuation"), |c| match_chs(c, Punct::CHARSET)) {
            builder.push(TokenPunct {
                span: start_span,
                ch: Punct::new(ch),
                glued: builder.glued_punct().is_some(),
            });

            continue;
        }

        // Parse whitespace.
        if p.expect(symbol!("whitespace"), |c| {
            c.eat().is_some_and(|c| c.is_whitespace())
        }) {
            builder.push_space();
            continue;
        }

        // We're stuck :(
        let err = p.stuck();
        let c = p.recover(err);

        c.eat();

        builder.push_space();
    }

    builder.stream
}

fn parse_ident(p: P) -> Option<Ident> {
    let start = p.span();

    let first_ch = p.expect(symbol!("identifier"), |c| {
        c.eat().filter(|c| c.is_xid_start())
    })?;

    let mut accum = String::new();

    let raw = if first_ch == 'r' && p.expect(symbol!("#"), |c| match_ch(c, '#')) {
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

    Some(Ident {
        span: start.until(p.span()),
        text: Symbol::new(&accum),
        raw,
    })
}

fn match_ch(c: C, ch: char) -> bool {
    c.lookahead(|c| c.eat() == Some(ch))
}

fn match_chs(c: C, set: &str) -> Option<char> {
    c.lookahead(|c| c.eat().filter(|&v| set.contains(v)))
}
