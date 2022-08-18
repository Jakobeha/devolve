use logos::{Lexer, Logos};

use crate::graph::error::ParseErrorBody;

pub trait LexerExt {
    type Token;

    fn munch<R>(&mut self, desc: &'static str, expected: impl Fn(Self::Token) -> Option<R>) -> Result<R, (usize, ParseErrorBody)>;
    fn munch_end(&mut self) -> Result<(), (usize, ParseErrorBody)>;
}

impl<'a, Token: Logos<'a>> LexerExt for Lexer<'a, Token> {
    type Token = Token;

    fn munch<R>(&mut self, desc: &'static str, expected: impl Fn(Self::Token) -> Option<R>) -> Result<R, (usize, ParseErrorBody)> {
        match self.next() {
            None => Err((self.span().end, ParseErrorBody::ExpectedMore(desc))),
            Some(token) => expected(token).ok_or((self.span().start, ParseErrorBody::Expected(desc)))
        }
    }

    fn munch_end(&mut self) -> Result<(), (usize, ParseErrorBody)> {
        match self.next() {
            None => Ok(()),
            Some(_) => Err((self.span().start, ParseErrorBody::ExpectedLess))
        }
    }
}

