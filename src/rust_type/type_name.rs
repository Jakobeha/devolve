use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::iter::{empty, once};
use std::num::{ParseFloatError, ParseIntError, TryFromIntError};
use join_lazy_fmt::Join;
use derive_more::{Display, Error};
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RustTypeName {
    Ident {
        qualifiers: Vec<String>,
        simple_name: String,
        generic_args: Vec<RustTypeName>
    },
    Anonymous {
        desc: Cow<'static, str>
    },
    ConstExpr {
        code_as_string: String
    },
    Pointer {
        refd: Box<RustTypeName>,
        ptr_kind: RustPointerKind
    },
    Tuple {
        elems: Vec<RustTypeName>
    },
    Array {
        elem: Box<RustTypeName>,
        length: usize
    },
    Slice {
        elem: Box<RustTypeName>
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RustPointerKind {
    ImmRef,
    MutRef,
    ImmPtr,
    MutPtr
}

pub struct RustTypeNameQualified<'a>(&'a RustTypeName);

pub struct RustTypeNameUnqualified<'a>(&'a RustTypeName);

pub struct RustTypeNameDisplay<'a, 'b> {
    type_name: &'a RustTypeName,
    qualify: RustTypeNameDisplayQualify<'b>
}

#[derive(Clone, Copy)]
pub enum RustTypeNameDisplayQualify<'b> {
    Never,
    Always,
    Unambiguous {
        simple_names_in_scope: &'b SimpleNamesInScope
    }
}

pub type SimpleNamesInScope = HashSet<String>;

impl RustTypeName {
    pub fn scoped_simple(qualifiers: Vec<String>, simple_name: String) -> RustTypeName {
        RustTypeName::Ident {
            qualifiers,
            simple_name,
            generic_args: Vec::new()
        }
    }

    pub fn simple(simple_name: String) -> RustTypeName {
        RustTypeName::Ident {
            qualifiers: Vec::new(),
            simple_name,
            generic_args: Vec::new()
        }
    }

    pub fn primitive(name: &str) -> RustTypeName {
        Self::simple(String::from(name))
    }

    pub fn unknown() -> RustTypeName {
        RustTypeName::Anonymous {
            desc: Cow::Borrowed("unknown")
        }
    }

    pub fn bottom() -> RustTypeName {
        RustTypeName::Anonymous {
            desc: Cow::Borrowed("bottom")
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            RustTypeName::Anonymous { desc } => desc == "unknown",
            _ => false
        }
    }

    pub fn is_bottom(&self) -> bool {
        match self {
            RustTypeName::Anonymous { desc } => desc == "bottom",
            _ => false
        }
    }

    pub fn is_anonymous(&self) -> bool {
        matches!(self, RustTypeName::Anonymous { .. })
    }

    /// Convert generic parameters in idents to `{unknown}`, ignore everything else.
    /// Useful e.g. so you can register types like `Box<{unknown}>` if you know the size and alignment.
    pub fn erase_generics(&mut self) {
        if let RustTypeName::Ident { qualifiers: _, simple_name: _, generic_args } = self {
            for generic_arg in generic_args.iter_mut() {
                *generic_arg = RustTypeName::unknown();
            }
        }
    }

    pub fn iter_snis(&self) -> impl Iterator<Item=&str> {
        match self {
            RustTypeName::Ident {
                qualifiers: _,
                simple_name,
                generic_args
            } => {
                Box::new(once(simple_name.as_str()).chain(
                    generic_args.iter().flat_map(|arg| arg.iter_snis())
                )) as Box<dyn Iterator<Item=&str>>
            }
            RustTypeName::Anonymous { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&str>>,
            RustTypeName::ConstExpr { .. } => Box::new(empty()) as Box<dyn Iterator<Item=&str>>,
            RustTypeName::Pointer { ptr_kind: _, refd } => refd.iter_snis(),
            RustTypeName::Tuple { elems } => Box::new(
                elems.iter().flat_map(|elem| elem.iter_snis())
            ) as Box<dyn Iterator<Item=&str>>,
            RustTypeName::Array { elem, length: _ } => elem.iter_snis(),
            RustTypeName::Slice { elem } => elem.iter_snis()
        }
    }

    pub fn qualified(&self) -> RustTypeNameDisplay<'_, 'static> {
        RustTypeNameDisplay {
            type_name: self,
            qualify: RustTypeNameDisplayQualify::Always
        }
    }

    pub fn unqualified(&self) -> RustTypeNameDisplay<'_, 'static> {
        RustTypeNameDisplay {
            type_name: self,
            qualify: RustTypeNameDisplayQualify::Always
        }
    }

    /// Qualifies the type name if its simple name is in the set
    pub fn display<'a, 'b>(&'a self, snis: &'b SimpleNamesInScope) -> RustTypeNameDisplay<'a, 'b> {
        RustTypeNameDisplay {
            type_name: self,
            qualify: RustTypeNameDisplayQualify::Unambiguous {
                simple_names_in_scope: snis
            }
        }
    }
}

// region printing
impl<'a, 'b> Display for RustTypeNameDisplay<'a, 'b> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let display = |type_name: &'a RustTypeName| -> RustTypeNameDisplay<'a, 'b> {
            RustTypeNameDisplay {
                type_name,
                qualify: self.qualify
            }
        };
        match &self.type_name {
            RustTypeName::Ident {
                simple_name,
                qualifiers,
                generic_args
            } => {
                if self.qualify.do_qualify(simple_name) {
                    for qualifier in qualifiers {
                        write!(f, "{}::", qualifier)?;
                    }
                }
                write!(f, "{}", simple_name)?;
                if !generic_args.is_empty() {
                    write!(f, "<{}>", ", ".join(generic_args.iter().map(display)))?;
                }
                Ok(())
            }
            RustTypeName::Anonymous { desc } => write!(f, "{{{}}}", desc),
            RustTypeName::ConstExpr { code_as_string } => write!(f, "{}", code_as_string),
            RustTypeName::Pointer {
                ptr_kind,
                refd
            } => write!(f, "{}{}", ptr_kind, display(refd)),
            RustTypeName::Tuple { elems } => write!(f, "({})", ", ".join(elems.iter().map(display))),
            RustTypeName::Array { elem, length } => write!(f, "[{}; {}]", display(elem), length),
            RustTypeName::Slice { elem } => write!(f, "[{}]", display(elem))
        }
    }
}

impl Display for RustPointerKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RustPointerKind::ImmRef => write!(f, "&"),
            RustPointerKind::MutRef => write!(f, "&mut "),
            RustPointerKind::ImmPtr => write!(f, "*const "),
            RustPointerKind::MutPtr => write!(f, "&mut ")
        }
    }
}

impl<'a> RustTypeNameDisplayQualify<'a> {
    fn do_qualify(&self, simple_name: &str) -> bool {
        match self {
            RustTypeNameDisplayQualify::Never => false,
            RustTypeNameDisplayQualify::Always => true,
            RustTypeNameDisplayQualify::Unambiguous { simple_names_in_scope } => {
                simple_names_in_scope.contains(simple_name)
            }
        }
    }
}
// endregion

// region parsing
#[derive(Debug, Display, Error)]
#[display(fmt = "parse error at {}: {}", index, cause)]
pub struct RustTypeNameParseError {
    pub index: usize,
    pub cause: RustTypeNameParseErrorCause
}

#[derive(Debug, Display, Error)]
pub enum RustTypeNameParseErrorCause {
    IntegerParseError(#[error(source)] ParseIntError),
    ArrayIntegerParseError(#[error(source)] TryFromIntError),
    FloatParseError(#[error(source)] ParseFloatError),
    #[display(fmt = "unexpected token: {}", _0)]
    Unexpected(#[error(not(source))] String),
    #[display(fmt = "expected comma or close, got {}", _0)]
    ExpectedCommaOrClose(#[error(not(source))] String),
    #[display(fmt = "expected semicolon or close, got {}", _0)]
    ExpectedSemicolonOrClose(#[error(not(source))] String),
    #[display(fmt = "expected more")]
    ExpectedMore
}

#[derive(Logos)]
pub(crate) enum RustTypeNameToken {
    #[token("*const")]
    ImmPtr,
    #[token("*mut")]
    MutPtr,
    #[token("&mut", priority = 2)]
    MutRef,

    #[token("::")]
    DoubleColon,

    #[regex("[~!@#$%^&*-=+|:;,.?/(\\[{<>}\\])]", |lex| lex.slice().chars().next().unwrap())]
    Punct(char),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    #[regex("`[^`]*`")]
    Ident,

    #[regex("-?[0-9]+", priority = 2, callback = |lex| lex.slice().parse::<i64>())]
    Integer(Result<i64, ParseIntError>),
    #[regex("-?[0-9]+\\.[0-9]*", |lex| lex.slice().parse::<f64>())]
    #[regex("-?[0-9]+\\.[0-9]+e[0-9]+", |lex| lex.slice().parse::<f64>())]
    #[regex("-?[0-9]+e[0-9]+", |lex| lex.slice().parse::<f64>())]
    Float(Result<f64, ParseFloatError>),
    #[regex("\"([^\"]|\\\\\")*\"")]
    String,

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl<'a> TryFrom<&'a str> for RustTypeName {
    type Error = RustTypeNameParseError;

    fn try_from(str: &'a str) -> Result<Self, Self::Error> {
        let mut lexer = Lexer::<RustTypeNameToken>::new(str);
        RustTypeName::parse_from(&mut lexer, true)
    }
}

enum RustTypeNameParseState {
    Init,
    AfterIdent {
        qualifiers: Vec<String>,
        simple_name: String
    },
    ExpectsIdent {
        qualifiers: Vec<String>
    },
    Done {
        result: RustTypeName
    }
}

impl RustTypeName {
    pub(crate) fn parse_from(lexer: &mut Lexer<'_, RustTypeNameToken>, parse_eof: bool) -> Result<Self, RustTypeNameParseError> {
        let mut state = RustTypeNameParseState::Init;
        let mut ptr_stack = Vec::new();
        fn unexpected(lexer: &Lexer<'_, RustTypeNameToken>) -> RustTypeNameParseError {
            RustTypeNameParseError {
                index: lexer.span().start,
                cause: RustTypeNameParseErrorCause::Unexpected(lexer.slice().to_string())
            }
        }
        fn expected_comma_or_close(lexer: &Lexer<'_, RustTypeNameToken>) -> RustTypeNameParseError {
            RustTypeNameParseError {
                index: lexer.span().start,
                cause: RustTypeNameParseErrorCause::ExpectedCommaOrClose(lexer.slice().to_string())
            }
        }
        fn expected_semicolon_or_close(lexer: &Lexer<'_, RustTypeNameToken>) -> RustTypeNameParseError {
            RustTypeNameParseError {
                index: lexer.span().start,
                cause: RustTypeNameParseErrorCause::ExpectedSemicolonOrClose(lexer.slice().to_string())
            }
        }
        fn unexpected_end(lexer: &Lexer<'_, RustTypeNameToken>) -> RustTypeNameParseError {
            RustTypeNameParseError {
                index: lexer.span().end,
                cause: RustTypeNameParseErrorCause::ExpectedMore
            }
        }
        while let Some(token) = lexer.next() {
            state = match state {
                RustTypeNameParseState::Init => match token {
                    RustTypeNameToken::Ident => RustTypeNameParseState::AfterIdent {
                        qualifiers: Vec::new(),
                        simple_name: lexer.slice().to_string()
                    },
                    RustTypeNameToken::ImmPtr => {
                        ptr_stack.push(RustPointerKind::ImmPtr);
                        RustTypeNameParseState::Init
                    }
                    RustTypeNameToken::MutPtr => {
                        ptr_stack.push(RustPointerKind::MutPtr);
                        RustTypeNameParseState::Init
                    }
                    RustTypeNameToken::MutRef => {
                        ptr_stack.push(RustPointerKind::MutRef);
                        RustTypeNameParseState::Init
                    }
                    RustTypeNameToken::Punct('&') => {
                        ptr_stack.push(RustPointerKind::ImmRef);
                        RustTypeNameParseState::Init
                    }
                    RustTypeNameToken::Punct('(') => {
                        let mut elems = Vec::new();
                        while !lexer.remainder().trim_start().starts_with(')') {
                            elems.push(RustTypeName::parse_from(lexer, false)?);
                            match lexer.next() {
                                Some(RustTypeNameToken::Punct(')')) => break,
                                Some(RustTypeNameToken::Punct(',')) => {},
                                Some(_) => return Err(expected_comma_or_close(lexer)),
                                None => return Err(unexpected_end(lexer))
                            }
                        }
                        RustTypeNameParseState::Done {
                            result: RustTypeName::Tuple { elems }
                        }
                    }
                    RustTypeNameToken::Punct('[') => {
                        let elem = Box::new(RustTypeName::parse_from(lexer, false)?);
                        match lexer.next() {
                            Some(RustTypeNameToken::Punct(']')) => RustTypeNameParseState::Done {
                                result: RustTypeName::Slice { elem }
                            },
                            Some(RustTypeNameToken::Punct(';')) => match lexer.next() {
                                Some(RustTypeNameToken::Integer(integer)) => match integer {
                                    Ok(integer) => match usize::try_from(integer) {
                                        Ok(length) => match lexer.next() {
                                            Some(RustTypeNameToken::Punct(']')) => RustTypeNameParseState::Done {
                                                result: RustTypeName::Array { elem, length }
                                            },
                                            Some(_) => return Err(unexpected(lexer)),
                                            None => return Err(unexpected_end(lexer))
                                        },
                                        Err(error) => return Err(RustTypeNameParseError {
                                            index: lexer.span().start,
                                            cause: RustTypeNameParseErrorCause::ArrayIntegerParseError(error)
                                        })
                                    },
                                    Err(err) => return Err(RustTypeNameParseError {
                                        index: lexer.span().start,
                                        cause: RustTypeNameParseErrorCause::IntegerParseError(err)
                                    })
                                },
                                Some(_) => return Err(unexpected(lexer)),
                                None => return Err(unexpected_end(lexer))
                            },
                            Some(_) => return Err(expected_semicolon_or_close(lexer)),
                            None => return Err(unexpected_end(lexer))
                        }
                    },
                    RustTypeNameToken::Punct('{') => match lexer.next() {
                        Some(RustTypeNameToken::Ident) => {
                            let desc = lexer.slice().to_string();
                            match lexer.next() {
                                Some(RustTypeNameToken::Punct('}')) => RustTypeNameParseState::Done {
                                    result: RustTypeName::Anonymous { desc: Cow::Owned(desc) }
                                },
                                Some(_) => return Err(unexpected(lexer)),
                                None => return Err(unexpected_end(lexer))
                            }
                        },
                        _ => return Err(unexpected(lexer))
                    },
                    RustTypeNameToken::Integer(integer) => match integer {
                        Ok(integer) => RustTypeNameParseState::Done {
                            result: RustTypeName::ConstExpr {
                                code_as_string: integer.to_string()
                            }
                        },
                        Err(error) => return Err(RustTypeNameParseError {
                            index: lexer.span().end,
                            cause: RustTypeNameParseErrorCause::IntegerParseError(error)
                        })
                    },
                    RustTypeNameToken::Float(float) => match float {
                        Ok(float) => RustTypeNameParseState::Done {
                            result: RustTypeName::ConstExpr {
                                code_as_string: float.to_string()
                            }
                        },
                        Err(error) => return Err(RustTypeNameParseError {
                            index: lexer.span().end,
                            cause: RustTypeNameParseErrorCause::FloatParseError(error)
                        })
                    },
                    RustTypeNameToken::String => {
                        RustTypeNameParseState::Done {
                            result: RustTypeName::ConstExpr {
                                code_as_string: lexer.slice().to_string()
                            }
                        }
                    }
                    _ => return Err(unexpected(lexer))
                }
                RustTypeNameParseState::AfterIdent {
                    mut qualifiers,
                    simple_name
                } => match token {
                    Some(RustTypeNameToken::DoubleColon) => {
                        qualifiers.push(simple_name);
                        RustTypeNameParseState::ExpectsIdent {
                            qualifiers
                        }
                    }
                    Some(RustTypeNameToken::Punct('<')) => {
                        let mut elems = Vec::new();
                        while !lexer.remainder().trim_start().starts_with('>') {
                            elems.push(RustTypeName::parse_from(lexer, false)?);
                            match lexer.next() {
                                Some(RustTypeNameToken::Punct('>')) => break,
                                Some(RustTypeNameToken::Punct(',')) => {},
                                Some(_) => return Err(expected_comma_or_close(lexer)),
                                None => return Err(unexpected_end(lexer))
                            }
                        }
                        RustTypeNameParseState::Done {
                            result: RustTypeName::Ident {
                                qualifiers,
                                simple_name,
                                generic_args: elems
                            }
                        }
                    }
                    Some(_) => return Err(unexpected(lexer)),
                    None => return Err(unexpected_end(lexer))
                }
                RustTypeNameParseState::ExpectsIdent {
                    qualifiers
                } => match token {
                    RustTypeNameToken::Ident => RustTypeNameParseState::AfterIdent {
                        qualifiers,
                        simple_name: lexer.slice().to_string()
                    },
                    _ => return Err(unexpected(lexer))
                }
                RustTypeNameParseState::Done { result: _ } => return Err(unexpected(lexer))
            };
            if !parse_eof {
                let peek_char = lexer.remainder().trim_start().chars().next();
                match peek_char {
                    ',' | ':' | ')' | ']' | '>' | '}' => break,
                    _ => {}
                }
            }
        }
        let mut result = match state {
            RustTypeNameParseState::AfterIdent {
                qualifiers,
                simple_name
            } => RustTypeName::Ident {
                qualifiers,
                simple_name,
                generic_args: Vec::new()
            },
            RustTypeNameParseState::Init |
            RustTypeNameParseState::ExpectsIdent { .. } => return Err(unexpected_end(lexer)),
            RustTypeNameParseState::Done { result } => result
        };
        for ptr_kind in ptr_stack {
            result = RustTypeName::Pointer {
                ptr_kind,
                refd: Box::new(result)
            }
        }
        Ok(result)
    }
}
// endregion