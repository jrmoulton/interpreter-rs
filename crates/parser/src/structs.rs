use error_stack::Report;
use lexer::Token;

use crate::{error::ParseError, statement::Statement};

// Extras

pub(crate) enum AssignLet {
    Assign,
    Let(Token),
}

#[derive(PartialEq)]
pub(crate) enum ArgState {
    Empty,
    Arg,
    Comma,
}
#[derive(PartialEq)]
pub(crate) enum ParamState {
    Empty,
    Ident,
    Comma,
}

pub(crate) trait ExtendAssign {
    fn extend_assign(&mut self, e: error_stack::Report<ParseError>);
}
impl ExtendAssign for Option<error_stack::Report<ParseError>> {
    fn extend_assign(&mut self, e: error_stack::Report<ParseError>) {
        if let Some(error) = self.as_mut() {
            error.extend_one(e);
        } else {
            *self = Some(e);
        }
    }
}

pub type ParseResult<T> = error_stack::Result<T, ParseError>;

pub(crate) trait PushExtend {
    fn push_extend(self, statements: &mut Vec<Statement>, error: &mut Option<Report<ParseError>>);
}

impl PushExtend for ParseResult<Statement> {
    fn push_extend(self, statements: &mut Vec<Statement>, error: &mut Option<Report<ParseError>>) {
        match self {
            Ok(statement) => statements.push(statement),
            Err(e) => error.extend_assign(e),
        }
    }
}
