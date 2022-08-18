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
            Some(token) => if expected(token) {
                Ok(())
            } else {
                Err((self.span().start, ParseErrorBody::Expected(desc)))
            },
            None => Err((self.span().end, ParseErrorBody::ExpectedMore(desc)))
        }
    }

    fn munch_end(&mut self) -> Result<(), (usize, ParseErrorBody)> {
        match self.next() {
            None => Ok(()),
            Some(_) => Err((self.span().start, ParseErrorBody::ExpectedLess))
        }
    }
}

