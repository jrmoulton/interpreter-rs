use std::{error::Error, fmt::Display};

use lexer::Token;
use owo_colors::OwoColorize;

use crate::{expr::Expr, statement::Statement};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedTerminatedExpr(Expr),
    ExpectedTerminatedStatement(Statement),
    UnexpectedStatement(Statement),
    Eof,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp = match self {
            ParseError::UnexpectedToken(lok_tok) => format!("Unexpected Token: {}", lok_tok.kind),
            ParseError::UnexpectedTerminatedExpr(expr) => {
                format!("Error: Unexpected token `;` after expression: {expr}")
            },
            ParseError::ExpectedTerminatedStatement(statement) => {
                format!("Error: Expected token ';' after statement: {statement}")
            },
            ParseError::UnexpectedStatement(statement) => {
                format!("Error: Unexpected statement: {statement}")
            },
            ParseError::Eof => "Error: Unexpected end of input".into(),
        };
        f.write_str(&temp)
    }
}
impl Error for ParseError {}

pub struct Suggestion(pub &'static str);
impl Display for Suggestion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&if cfg!(not(test)) {
            format!("Suggestion: {}", self.0.bright_green().bold())
        } else {
            format!("Suggestion: {}", self.0)
        })
    }
}
pub struct Help(pub &'static str);
impl Display for Help {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&if cfg!(not(test)) {
            format!("Help: {}", self.0.bright_blue().bold())
        } else {
            format!("Help: {}", self.0)
        })
    }
}
pub const SEMI_SUGGEST: Suggestion = Suggestion("Add a semicolon after the expression");
pub const EXPR_STATEMENT: Help =
    Help("If expressions are not the end of the input they should be terminated with semicolons");
