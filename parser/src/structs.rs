use lexer::PeekLex;
use lexer::{Span, Token};
use owo_colors::OwoColorize;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Write;

use crate::expect_peek;

pub trait HasSpan {
    fn get_span(&self) -> Span;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Let {
        ident: Token,
        expr: ExprBase,
        span: Span,
    },
    Assign {
        ident: Token,
        expr: ExprBase,
        span: Span,
    },
    Return {
        expr: Option<ExprBase>,
        span: Span,
    },
    Expression {
        expr: ExprBase,
        terminated: bool,
        span: Span,
    },
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp = match self {
            Statement::Let { ident, expr, .. } => format!("let {} = {}", ident, expr),
            Statement::Return { expr, .. } => match expr {
                Some(expr) => format!("return {expr};"),
                None => "return;".into(),
            },
            Statement::Expression { expr, .. } => format!("{expr}"),
            Statement::Assign { ident, expr, .. } => format!("{} = {}", ident, expr),
        };
        f.write_str(&temp)
    }
}
impl HasSpan for Statement {
    fn get_span(&self) -> Span {
        match self {
            Statement::Let { span, .. } => *span,
            Statement::Assign { span, .. } => *span,
            Statement::Return { span, .. } => *span,
            Statement::Expression { expr, .. } => expr.get_span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprBase {
    IntLiteral(Token),
    BoolLiteral(Token),
    StringLiteral(Token),
    Identifier(Token),
    Array {
        exprs: Vec<ExprBase>,
        span: Span,
    },
    Func {
        parameters: Vec<Ident>,
        body: Scope,
        span: Span,
    },
    Call {
        function: Box<ExprBase>,
        args: Vec<ExprBase>,
        span: Span,
    },
    Scope {
        statements: Vec<Statement>,
        span: Span,
    },
    Prefix {
        operator: Token,
        expression: Box<ExprBase>,
        span: Span,
    },
    Binary {
        lhs: Box<ExprBase>,
        operator: Token,
        rhs: Box<ExprBase>,
        span: Span,
    },
    Index {
        array: Box<ExprBase>,
        index: Box<ExprBase>,
        span: Span,
    },
    MethodCall {
        /// An expression that can resolve to a value
        instance: Box<ExprBase>,
        /// an call expression that with the method ident comes afer the Dot
        method: Box<ExprBase>,
        span: Span,
    },
    If {
        condition: Box<ExprBase>,
        consequence: Scope,
        alternative: Option<ElseIfExpr>,
        span: Span,
    },
}
impl ExprBase {
    pub fn get_span(&self) -> Span {
        use ExprBase::*;
        match self {
            IntLiteral(token) | BoolLiteral(token) | StringLiteral(token) | Identifier(token) => {
                token.span
            }
            Func { span, .. }
            | Array { span, .. }
            | Call { span, .. }
            | Scope { span, .. }
            | Prefix { span, .. }
            | Binary { span, .. }
            | Index { span, .. }
            | If { span, .. }
            | MethodCall { span, .. } => *span,
        }
    }
}

impl Display for ExprBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret_str = match self {
            ExprBase::IntLiteral(lok_tok)
            | ExprBase::BoolLiteral(lok_tok)
            | ExprBase::StringLiteral(lok_tok) => {
                format!("{lok_tok}",)
            }
            ExprBase::Array { exprs, .. } => {
                let mut ret_str = String::from("[ ");
                for item in exprs.iter() {
                    write!(ret_str, "{item}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
            ExprBase::Index { array, index, .. } => {
                format!("{array}[{index}]")
            }
            ExprBase::MethodCall {
                instance, method, ..
            } => {
                format!("{instance}.{method}")
            }
            ExprBase::Func { parameters, .. } => {
                let mut ret_str = String::from("(");
                for param in parameters.iter() {
                    write!(ret_str, "{param}, ")?;
                }
                ret_str.push(')');
                format!("fn{ret_str}{{...}}")
            }
            ExprBase::Call { function, args, .. } => {
                let mut ret_str = String::from("[ ");
                for arg in args.iter() {
                    write!(ret_str, "{arg}, ")?;
                }
                ret_str.push_str(" ]");
                format!("Call: func{{{function}}}, args{{{ret_str}}}")
            }
            ExprBase::Identifier(ident) => format!("{ident}"),
            ExprBase::Scope { statements, .. } => {
                let mut ret_str = String::from("[ ");
                for statement in statements.iter() {
                    write!(ret_str, "{statement}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
            ExprBase::Prefix {
                operator,
                expression,
                ..
            } => {
                format!("{operator}:{}", expression)
            }
            ExprBase::Binary {
                lhs, operator, rhs, ..
            } => {
                format!("({} {} {})", lhs, operator, rhs)
            }
            ExprBase::If { condition, .. } => {
                format!("{condition}")
            }
        };
        f.write_str(&ret_str)
    }
}

// Sub types

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    pub statements: Vec<Statement>,
    pub span: Span,
}
impl IntoIterator for Scope {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Statement>;
    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}
impl From<Scope> for ExprBase {
    fn from(scope: Scope) -> Self {
        ExprBase::Scope {
            statements: scope.statements,
            span: scope.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub condition: Box<ExprBase>,
    pub consequence: Scope,
    pub alternative: Option<ElseIfExpr>,
    pub span: Span,
}
impl From<IfExpr> for ExprBase {
    fn from(if_expr: IfExpr) -> Self {
        ExprBase::If {
            condition: if_expr.condition,
            consequence: if_expr.consequence,
            alternative: if_expr.alternative,
            span: if_expr.span,
        }
    }
}

/// The optional alterantive of an if expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElseIfExpr {
    ElseIf(Box<IfExpr>),
    Else(Scope),
}
impl ElseIfExpr {
    pub fn get_span(&self) -> Span {
        match self {
            ElseIfExpr::ElseIf(if_expr) => if_expr.span,
            ElseIfExpr::Else(scope) => scope.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub Token);
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

// Errors

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedTerminatedExpr(ExprBase),
    ExpectedTerminatedExpr(ExprBase),
    MultipleUnterminatedExpressions,
    Eof,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp = match self {
            ParseError::UnexpectedToken(lok_tok) => format!("Unexpected Token: {}", lok_tok.kind),
            ParseError::UnexpectedTerminatedExpr(expr) => {
                format!("Unexpected token `;` after expression: {expr}")
            }
            ParseError::ExpectedTerminatedExpr(expr) => {
                format!("Expected token ';' after expression: {expr}")
            }
            ParseError::MultipleUnterminatedExpressions => {
                ("Error: Multiple Unterminated Expressions").to_string()
            }
            ParseError::Eof => "Unexpected end of input".into(),
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

// Extras

pub(crate) enum TermState {
    None,
    Term,
    NonTerm,
}
impl TermState {
    pub fn handle_new(
        &mut self,
        lexer: &mut PeekLex,
        statement: ExprBase,
        statements: &mut Vec<Statement>,
        error: &mut Option<error_stack::Report<ParseError>>,
    ) {
        // Matching on the temination state improves error messages and hnadling
        match self {
            TermState::None | TermState::Term => {
                *self = match expect_peek(lexer, lexer::TokenKind::Semicolon) {
                    Ok(_) => TermState::Term,
                    Err(_) => TermState::NonTerm,
                };
                statements.push(Statement::Expression(statement));
            }
            TermState::NonTerm => match expect_peek(lexer, lexer::TokenKind::Semicolon) {
                Ok(_) => statements.push(Statement::Expression(statement)),
                Err(_) => {
                    let e = error_stack::Report::new(ParseError::MultipleUnterminatedExpressions)
                        .attach(SEMI_SUGGEST)
                        .attach_printable(crate::pretty_output(
                            statement.get_span(),
                            lexer.get_input(),
                        ));
                    error.extend_assign(e);
                }
            },
        }
    }
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
