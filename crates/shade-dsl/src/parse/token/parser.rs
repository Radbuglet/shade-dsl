use unicode_xid::UnicodeXID;

use crate::{
    base::{CharCursor, CharParser, Diag, Gcx, LeafDiag, Parser, Span, SpanCharCursor, Symbol},
    symbol,
};

use super::{
    GroupDelimiter, Ident, Punct, StrLitKind, TokenCharLit, TokenGroup, TokenPunct, TokenStrLit,
    TokenStream, TokenTree,
};

type P<'a, 'gcx, 'ch> = &'a mut CharParser<'gcx, 'ch>;
type C<'a, 'gcx, 'ch> = &'a mut CharCursor<'ch>;

pub fn tokenize(gcx: Gcx<'_>, span: Span) -> TokenGroup {
    let text = gcx.source_map.file(span.lo).text(span);
    let mut parser = Parser::new(gcx, SpanCharCursor::new(span, &text));

    parser.context(symbol!("tokenizing the file"), |p| {
        parse_group(p, p.span(), GroupDelimiter::File)
    })
}

#[derive(Default)]
struct GroupBuilder {
    stream: TokenStream,
    glued: bool,
}

impl GroupBuilder {
    fn push(&mut self, token: impl Into<TokenTree>) {
        self.glued = true;
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

    fn glued_ident(&self) -> Option<&Ident> {
        self.stream
            .last()
            .and_then(|v| v.ident())
            .filter(|_| self.glued)
    }
}

fn parse_group(p: P, group_start: Span, delimiter: GroupDelimiter) -> TokenGroup {
    let mut builder = GroupBuilder::default();

    'parse: loop {
        let token_start = p.span();

        // Parse closing group delimiters
        if let Some(closing_del) = p.expect(delimiter.closing_name(), |c| {
            GroupDelimiter::CLOSEABLE
                .iter()
                .copied()
                .find(|v| match_ch_or_eof(c, v.closing()))
        }) {
            if closing_del != delimiter {
                p.dcx().emit(Diag::span_err(
                    token_start,
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
                builder.push(parse_group(p, token_start, open_del));

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

        // Parse identifiers (and prefixed string literals)
        if let Some(ident) = parse_ident(p) {
            // Try to parse a string literal
            let mut pounds = 0;

            while p.expect(symbol!("`#`"), |c| match_ch(c, '#')) {
                pounds += 1;
            }

            if let Some(str) = parse_string_lit(p, &builder, Some(ident), pounds, token_start) {
                builder.push(str);
                continue;
            }

            // Otherwise, parse it as an identifier.
            if pounds == 0 {
                // Parse identifier
                builder.push(ident);
                continue;
            } else {
                // Has to be a string :(
                // Recovery strategy: parse the expected string literal as a regular token.
                let _ = p.stuck();
                continue;
            }
        }

        // Parse un-prefixed string literals
        if let Some(str) = parse_string_lit(p, &builder, None, 0, token_start) {
            builder.push(str);
            continue;
        }

        // Parse character
        if let Some(ch) = parse_char_lit(p) {
            builder.push(ch);
            continue;
        }

        // Parse numeric literals
        // TODO

        // Parse punctuation
        if let Some(ch) = p.expect(symbol!("punctuation"), |c| match_chs(c, Punct::CHARSET)) {
            builder.push(TokenPunct {
                span: token_start,
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

    TokenGroup {
        span: group_start.until(p.span()),
        delimiter,
        tokens: builder.stream,
    }
}

fn parse_ident(p: P) -> Option<Ident> {
    let start = p.span();

    let first_ch = p.expect(symbol!("identifier"), |c| {
        c.eat().filter(|c| c.is_xid_start())
    })?;

    let mut accum = String::new();

    let raw = if first_ch == '@' {
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

fn parse_string_lit(
    p: P,
    builder: &GroupBuilder,
    prefix: Option<Ident>,
    mut pounds: u32,
    prefix_start: Span,
) -> Option<TokenStrLit> {
    let quote_start = p.span();

    // Match the opening quote
    if !p.expect(symbol!("`\"`"), |c| match_ch(c, '"')) {
        return None;
    }

    if let Some(ident) = builder.glued_ident() {
        // Recovery strategy: continue trying to parse this string as UTF-8
        let _ = p.err(Diag::span_err(ident.span, "invalid prefix"));
    }

    // Interpret the prefix
    let (kind, is_raw) = 'prefix: {
        let Some(prefix) = prefix else {
            break 'prefix (StrLitKind::Utf8Slice, false);
        };

        if prefix.raw {
            // Recovery strategy: continue trying to parse this string as UTF-8
            let _ = p.err(
                Diag::span_err(prefix.span, "unknown string literal prefix").child(
                    LeafDiag::span_note(
                        prefix.span.truncate_left(1),
                        "`@`, the raw identifier indicator, is not expected in any string prefix",
                    ),
                ),
            );

            break 'prefix (StrLitKind::Utf8Slice, false);
        }

        let (prefix_sym, is_raw) = if let Some(prefix_sym) =
            prefix.text.as_str(|v| v.strip_prefix('r').map(Symbol::new))
        {
            (prefix_sym, true)
        } else {
            (prefix.text, false)
        };

        let Some(lit_kind) = StrLitKind::VARIANTS
            .into_iter()
            .find(|v| v.prefix() == prefix_sym)
        else {
            // Recovery strategy: continue trying to parse this string as UTF-8
            let _ = p.err(Diag::span_err(
                prefix.span,
                format_args!("unknown string literal prefix `{prefix_sym}`"),
            ));

            break 'prefix (StrLitKind::Utf8Slice, false);
        };

        (lit_kind, is_raw)
    };

    if pounds > 0 && !is_raw {
        // Recovery strategy: try parsing this string as if it didn't have pounds.
        let _ = p.err(Diag::span_err(
            prefix_start.until(quote_start),
            "cannot surround a string literal with `#`s when the string is not raw",
        ));

        pounds = 0;
    }

    // Parse the string contents
    let mut accum = String::new();

    let closing_name = if pounds == 0 {
        symbol!("`\"`")
    } else {
        let mut accum = String::new();
        accum.push_str("`\"");
        for _ in 0..pounds {
            accum.push('#');
        }
        accum.push('`');
        Symbol::new(&accum)
    };

    loop {
        // Match closing quote
        if p.expect(closing_name, |c| {
            if !match_ch(c, '"') {
                return false;
            }

            for _ in 0..pounds {
                if !match_ch(c, '#') {
                    return false;
                }
            }

            true
        }) {
            break;
        }

        // Match escape
        if !is_raw {
            if let Some(ch) = parse_char_escape(p) {
                accum.push(ch);
                continue;
            }
        }

        // Match regular character
        if let Some(ch) = parse_regular_char(p) {
            accum.push(ch);
            continue;
        }

        // Recovery strategy: parse this unexpected character (EOF, probably) as a token.
        let _ = p.stuck();

        break;
    }

    Some(TokenStrLit {
        span: prefix_start.until(p.span()),
        kind,
        value: Symbol::new(&accum),
    })
}

fn parse_char_lit(p: P) -> Option<TokenCharLit> {
    let start = p.span();

    if !p.expect(symbol!("`'`"), |c| match_ch(c, '\'')) {
        return None;
    }

    let value = 'parse_ch: {
        if let Some(ch) = parse_char_escape(p) {
            break 'parse_ch ch;
        }

        if let Some(ch) = parse_regular_char(p) {
            break 'parse_ch ch;
        }

        // Recovery strategy: parse this unexpected character (EOF, probably) as a token.
        let _ = p.stuck();

        '?'
    };

    if !p.expect(symbol!("`'`"), |c| match_ch(c, '\'')) {
        // Recovery strategy: parse this unexpected character as the next token.
        let _ = p.stuck();

        // (fallthrough)
    }

    Some(TokenCharLit {
        span: start.until(p.span()),
        value,
    })
}

fn parse_char_escape(p: P) -> Option<char> {
    let start = p.span();

    if !p.expect(symbol!("`\\`"), |c| match_ch(c, '\\')) {
        return None;
    }

    // Match well-known escapes
    let well_known = [
        (symbol!("`r`"), 'r', '\r'),
        (symbol!("`n`"), 'n', '\n'),
        (symbol!("`t`"), 't', '\t'),
        (symbol!("`0`"), '0', '\0'),
        (symbol!("`\"`"), '"', '"'),
        (symbol!("`'`"), '\'', '\''),
    ];

    for (name, ch, escape) in well_known {
        if p.expect(name, |c| match_ch(c, ch)) {
            return Some(escape);
        }
    }

    // Match ASCII escapes
    if p.expect(symbol!("`x`"), |c| match_ch(c, 'x')) {
        let Some(hexits) = p.expect(symbol!("hexadecimal ASCII code"), |c| {
            Some([
                c.eat().filter(|c| c.is_ascii_hexdigit())?,
                c.eat().filter(|c| c.is_ascii_hexdigit())?,
            ])
        }) else {
            // Recovery strategy: parse the unexpected hexcode as regular string characters.
            let _ = p.stuck();

            return None;
        };

        let hex_seq = [hexits[0] as u8, hexits[1] as u8];
        let hex_seq = std::str::from_utf8(&hex_seq).unwrap(); // Hexits are ASCII

        let code = u8::from_str_radix(hex_seq, 16).unwrap();

        if code > 0x7F {
            // Recovery strategy: pretend the code was valid but substitute it with a placeholder
            // character.
            let _ = p.err(Diag::span_err(
                start.until(p.span()),
                "ASCII escape code must be at most `\\x7F`",
            ));

            return Some('?');
        }

        return Some(code as char);
    }

    // Match Unicode escapes
    if p.expect(symbol!("`u`"), |c| match_ch(c, 'u')) {
        todo!()
    }

    // Recovery strategy: parse the unexpected escape as a regular string character.
    let _ = p.stuck();

    None
}

fn parse_regular_char(p: P) -> Option<char> {
    p.expect(symbol!("character"), |c| c.eat())
}

fn match_ch(c: C, ch: char) -> bool {
    match_ch_or_eof(c, Some(ch))
}

fn match_ch_or_eof(c: C, ch: Option<char>) -> bool {
    c.lookahead(|c| c.eat() == ch)
}

fn match_chs(c: C, set: &str) -> Option<char> {
    c.lookahead(|c| c.eat().filter(|&v| set.contains(v)))
}
