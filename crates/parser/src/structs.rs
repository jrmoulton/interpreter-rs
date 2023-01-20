use std::{
    error::Error,
    fmt::{Display, Write},
};

use error_stack::Report;
use lexer::{PeekLex, Span, Token};
use owo_colors::OwoColorize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Let { ident: String, expr: Expr, span: Span },
    Assign { ident: String, expr: Expr, span: Span },
    Return { expr: Option<Expr>, span: Span },
    Expression { expr: Expr, terminated: bool, span: Span },
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
pub enum Expr {
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
        exprs: Vec<Expr>,
        span: Span,
    },
    FuncDef {
        parameters: Vec<Ident>,
        body: Scope,
        span: Span,
    },
    FuncCall {
        function: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Scope {
        statements: Vec<Statement>,
        span: Span,
    },
    Prefix {
        operator: Token,
        expression: Box<Expr>,
        span: Span,
    },
    Binary {
        lhs: Box<Expr>,
        operator: Token,
        rhs: Box<Expr>,
        span: Span,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    MethodCall {
        /// An expression that can resolve to a value
        instance: Box<Expr>,
        /// an call expression that with the method ident comes afer the Dot
        method: Box<Expr>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        consequence: Scope,
        alternative: Option<ElseIfExpr>,
        span: Span,
    },
}
impl Expr {
    pub fn get_span(&self) -> Span {
        use Expr::*;
        match self {
            IntLiteral { span, .. }
            | BoolLiteral { span, .. }
            | StringLiteral { span, .. }
            | Identifier { span, .. }
            | FuncDef { span, .. }
            | Array { span, .. }
            | FuncCall { span, .. }
            | Scope { span, .. }
            | Prefix { span, .. }
            | Binary { span, .. }
            | Index { span, .. }
            | If { span, .. }
            | MethodCall { span, .. } => *span,
        }
    }
    pub fn into_statement(
        self, lexer: &mut PeekLex, error: &mut Option<Report<ParseError>>,
    ) -> Statement {
        let terminated = match crate::expect_peek(lexer, lexer::TokenKind::Semicolon) {
            Ok(_) => true,
            Err(_) => {
                match lexer.peek().cloned() {
                    Some(tok) if !matches!(tok.kind, lexer::TokenKind::RBrace) => {
                        error.extend_assign(
                            Report::new(ParseError::UnexpectedToken(tok))
                                .attach(EXPR_STATEMENT)
                                .attach(SEMI_SUGGEST),
                        );
                    },
                    _ => {},
                };
                false
            },
        };
        Statement::Expression {
            span: self.get_span(),
            expr: self,
            terminated,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret_str = match self {
            Expr::IntLiteral { val, .. } => format!("{val}"),
            Expr::BoolLiteral { val, .. } => format!("{val}"),
            Expr::StringLiteral { val, .. } => format!("\"{val}\""),
            Expr::Identifier { ident, .. } => ident.clone(),
            Expr::Array { exprs, .. } => {
                let mut ret_str = String::from("[ ");
                for item in exprs.iter() {
                    write!(ret_str, "{item}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            },
            Expr::Index { array, index, .. } => {
                format!("{array}[{index}]")
            },
            Expr::MethodCall { instance, method, .. } => {
                format!("{instance}.{method}")
            },
            Expr::FuncDef { parameters, .. } => {
                let mut ret_str = String::from("(");
                for param in parameters.iter() {
                    write!(ret_str, "{param}, ")?;
                }
                ret_str.push(')');
                format!("fn{ret_str}{{...}}")
            },
            Expr::FuncCall { function, args, .. } => {
                let mut ret_str = String::from("[ ");
                for arg in args.iter() {
                    write!(ret_str, "{arg}, ")?;
                }
                ret_str.push_str(" ]");
                format!("Call: func{{{function}}}, args{{{ret_str}}}")
            },
            Expr::Scope { statements, .. } => {
                let mut ret_str = String::from("[ ");
                for statement in statements.iter() {
                    write!(ret_str, "{statement}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            },
            Expr::Prefix { operator, expression, .. } => {
                format!("{operator}:{}", expression)
            },
            Expr::Binary { lhs, operator, rhs, .. } => {
                format!("({} {} {})", lhs, operator, rhs)
            },
            Expr::If { condition, .. } => {
                format!("{condition}")
            },
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
    type IntoIter = std::vec::IntoIter<Statement>;
    type Item = Statement;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}
impl From<Scope> for Expr {
    fn from(scope: Scope) -> Self {
        Expr::Scope {
            statements: scope.statements,
            span: scope.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub consequence: Scope,
    pub alternative: Option<ElseIfExpr>,
    pub span: Span,
}
impl From<IfExpr> for Expr {
    fn from(if_expr: IfExpr) -> Self {
        Expr::If {
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
pub struct Ident {
    pub ident: String,
    pub span: Span,
}
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.ident))
    }
}

// Errors

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
pub const PREV_SEMI_HELP: Help =
    Help("The statement before this one does not have a semicolon. Did you mean to add one?");
pub const EXPR_STATEMENT: Help =
    Help("If expressions are not the end of the input they should be terminated with semicolons");

// Extras

pub(crate) enum AssignLet {
    Assign,
    Let(Token),
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
