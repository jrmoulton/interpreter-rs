use error_stack::Report;
use lexer::{Span, Token};
use owo_colors::OwoColorize;
use std::error::Error;
use std::fmt::Display;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Let {
        ident: String,
        expr: ExprBase,
        span: Span,
    },
    Assign {
        ident: String,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprBase {
    IntLiteral {
        val: i64,
        span: Span,
    },
    BoolLiteral {
        val: bool,
        span: Span,
    },
    StringLiteral {
        val: String,
        span: Span,
    },
    Identifier {
        ident: String,
        span: Span,
    },
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
            IntLiteral { span, .. }
            | BoolLiteral { span, .. }
            | StringLiteral { span, .. }
            | Identifier { span, .. }
            | Func { span, .. }
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
            ExprBase::IntLiteral { val, .. } => format!("{val}"),
            ExprBase::BoolLiteral { val, .. } => format!("{val}"),
            ExprBase::StringLiteral { val, .. } => format!("\"{val}\""),
            ExprBase::Identifier { ident, .. } => ident.clone(),
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
            }
            ParseError::ExpectedTerminatedStatement(statement) => {
                format!("Error: Expected token ';' after statement: {statement}")
            }
            ParseError::UnexpectedStatement(statement) => {
                format!("Error: Unexpected statement: {statement}")
            }
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
pub const PREV_SEMI_HELP: Help =
    Help("The statement before this one does not have a semicolon. Did you mean to add one?");
// Extras

pub(crate) enum TermState {
    None,
    ClosedExpr,
    OpenExpr,
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

pub(crate) trait ESResultExt {
    fn handle_statement_err(
        self,
        lexer: &mut lexer::PeekLex,
        statements: &mut Vec<Statement>,
        error: &mut Option<Report<ParseError>>,
        term_state: &mut TermState,
    );
}

impl ESResultExt for ParseResult<Statement> {
    fn handle_statement_err(
        self,
        lexer: &mut lexer::PeekLex,
        statements: &mut Vec<Statement>,
        error: &mut Option<Report<ParseError>>,
        term_state: &mut TermState,
    ) {
        // if statements have an error still check for if it's terminated
        match self {
            Ok(mut statement) => match statement {
                Statement::Let { .. } | Statement::Assign { .. } | Statement::Return { .. } => {
                    match crate::expect_peek(lexer, lexer::TokenKind::Semicolon) {
                        Ok(_) => {
                            if matches!(term_state, TermState::OpenExpr) {
                                // if the statement is terminated and the prvious statement was not
                                error.extend_assign(
                                    Report::new(ParseError::UnexpectedStatement(statement))
                                        .attach(PREV_SEMI_HELP),
                                );
                            } else {
                                // if the statement is terminated and the prvious statement was also
                                statements.push(statement);
                            }
                        }
                        Err(_) => {
                            if matches!(term_state, TermState::OpenExpr) {
                                // if the statement is not terminated and the prvious statement was also not terminated
                                error.extend_assign(
                                    Report::new(ParseError::UnexpectedStatement(statement))
                                        .attach(PREV_SEMI_HELP),
                                );
                            } else {
                                // if the statement is not terminated and the prvious statement was terminated
                                error.extend_assign(
                                    Report::new(ParseError::ExpectedTerminatedStatement(statement))
                                        .attach(Help(
                                            "Statements should be terminated with semicolons",
                                        ))
                                        .attach(SEMI_SUGGEST),
                                );
                            }
                        }
                    };
                    *term_state = TermState::ClosedExpr;
                }
                Statement::Expression {
                    ref mut terminated, ..
                } => match crate::expect_peek(lexer, lexer::TokenKind::Semicolon) {
                    Ok(_) => {
                        if matches!(term_state, TermState::OpenExpr) {
                            // if the expression statement is terminated but the prvious statement was not
                            error.extend_assign(
                                Report::new(ParseError::UnexpectedStatement(statement))
                                    .attach(PREV_SEMI_HELP),
                            );
                        } else {
                            *terminated = true;
                            statements.push(statement);
                            *term_state = TermState::ClosedExpr;
                        }
                    }
                    Err(_) => {
                        if matches!(term_state, TermState::None | TermState::ClosedExpr) {
                            statements.push(statement);
                            *term_state = TermState::OpenExpr;
                        } else {
                            // if the expression statement is not terminated and the prvious statement was also not
                            error.extend_assign(
                                Report::new(ParseError::UnexpectedStatement(statement))
                                    .attach(PREV_SEMI_HELP),
                            );
                        }
                    }
                },
            },
            Err(e) => {
                error.extend_assign(e);
            }
        };
    }
}
