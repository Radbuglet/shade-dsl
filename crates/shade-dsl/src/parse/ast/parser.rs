use crate::parse::token::{TokenCursor, TokenParser};

type P<'a, 'gcx, 'g> = &'a mut TokenParser<'gcx, 'g>;
type C<'a, 'gcx, 'g> = &'a mut TokenCursor<'g>;

pub fn parse_module(p: P) {}
