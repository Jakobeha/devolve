use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use logos::{Lexer, Logos, Span, SpannedIter};
use derive_more::Display;
use snailquote::unescape;
use crate::graph::error::{ParseErrors, ParseError, ParseErrorBody};
use crate::misc::extract::extract;

pub trait LexerExt {
    type Token;

    fn munch<R>(&mut self, desc: &'static str, expected: impl Fn(Self::Token) -> Option<R>) -> Result<R, (usize, ParseErrorBody)>;
    fn munch_end(&mut self) -> Result<(), (usize, ParseErrorBody)>;
}

impl<Token> LexerExt for Lexer<Token> {
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

