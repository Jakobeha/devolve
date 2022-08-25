use std::fmt::{Display, Formatter};

use dui_graph::misc::fmt_with_ctx::{DisplayWithCtx, Indent};


pub macro error($errors:expr, $($t:tt)*) { {
    $errors.push(format!($($t)*));
} }

pub macro try_or_return($e:expr, $errors:expr, $msg:literal $(, $args:expr)*) {
    match $e {
        Ok(v) => v,
        Err(e) => {
            error!($errors, concat!($msg, ": {}") $(, $args)*, e);
            return
        }
    }
}

pub macro try_or_none($e:expr, $errors:expr, $msg:literal $(, $args:expr)*) {
    match $e {
        Ok(v) => Some(v),
        Err(e) => {
            error!($errors, concat!($msg, ": {}") $(, $args)*, e);
            None
        }
    }
}

pub struct ErrorNodes(Vec<ErrorNode>);

#[doc(hidden)]
pub enum ErrorNode {
    Leaf { message: String },
    Group(ErrorGroup)
}

#[doc(hidden)]
pub struct ErrorGroup {
    header: String,
    errors: ErrorNodes
}

impl<'a> From<&'a str> for ErrorNode {
    fn from(message: &'a str) -> Self {
        ErrorNode::Leaf { message: message.to_string() }
    }
}

impl From<String> for ErrorNode {
    fn from(message: String) -> Self {
        ErrorNode::Leaf { message }
    }
}

impl From<ErrorGroup> for ErrorNode {
    fn from(group: ErrorGroup) -> Self {
        ErrorNode::Group(group)
    }
}

impl ErrorNodes {
    pub fn log_errors_and_panic(fun: impl FnOnce(&mut ErrorNodes)) {
        let mut errors = ErrorNodes::new();
        fun(&mut errors);
        if errors.has_errors() {
            panic!("ERRORS:\n{}", errors);
        }
    }

    fn new() -> Self {
        ErrorNodes(Vec::new())
    }

    pub fn push(&mut self, item: impl Into<ErrorNode>) {
        self.0.push(item.into())
    }

    pub fn push_group<R>(&mut self, header: impl Into<String>, fun: impl FnOnce(&mut ErrorNodes) -> R) -> R {
        let mut errors = ErrorNodes::new();
        let result = fun(&mut errors);
        self.0.push(ErrorNode::Group(ErrorGroup {
            header: header.into(),
            errors
        }));
        result
    }

    pub fn has_errors(&self) -> bool {
        self.0.iter().any(|node| match node {
            ErrorNode::Leaf { .. } => true,
            ErrorNode::Group(group) => group.errors.has_errors()
        })
    }
}

impl DisplayWithCtx for ErrorNode {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        match self {
            ErrorNode::Leaf { message } => {
                let mut lines = message.lines();
                writeln!(f, "{}- {}", indent, lines.next().unwrap_or(""))?;
                for line in lines {
                    writeln!(f, "{}  {}", indent, line)?;
                }
                Ok(())
            },
            ErrorNode::Group(group) => {
                if group.errors.has_errors() {
                    writeln!(f, "{}* {}", indent, group.with_ctx(indent))
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl DisplayWithCtx for ErrorNodes {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter, indent: &Self::Ctx) -> std::fmt::Result {
        for node in &self.0 {
            write!(f, "{}", node.with_ctx(&indent.next()))?
        }
        Ok(())
    }
}

impl DisplayWithCtx for ErrorGroup {
    type Ctx = Indent;

    fn fmt(&self, f: &mut Formatter<'_>, indent: &Self::Ctx) -> std::fmt::Result {
        write!(f, "{}\n{}", self.header, self.errors.with_ctx(indent))
    }
}

impl Display for ErrorNodes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        DisplayWithCtx::fmt(self, f, &Indent::ZERO)
    }
}
