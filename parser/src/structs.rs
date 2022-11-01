use error_stack::Context;
use lexer::{Span, Token};
use owo_colors::OwoColorize;
use std::fmt::Display;
use std::fmt::Write;

#[derive(Debug, Clone)]
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
    Return(Option<Expr>),
    Expression(Expr),
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp = match self {
            Statement::Let { ident, expr, .. } => format!("let {} = {}", ident, expr),
            Statement::Return(return_statement) => match return_statement {
                Some(expr) => format!("return {expr};"),
                None => "return;".into(),
            },
            Statement::Expression(expression_statement) => format!("{expression_statement}"),
            Statement::Assign { ident, expr, .. } => format!("{} = {}", ident, expr),
        };
        f.write_str(&temp)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Terminated(ExprBase),
    NonTerminated(ExprBase),
}
impl Expr {
    pub fn expect_non_terminated(self) -> ExprBase {
        match self {
            Self::Terminated(_) => {
                panic!("Expected to be of type NonTerminated but found Terminated")
            }
            Self::NonTerminated(inner) => inner,
        }
    }
    pub fn expect_terminated(self) -> ExprBase {
        match self {
            Self::NonTerminated(_) => {
                panic!("Expected to be of type Terminated but found NonTerminated")
            }
            Self::Terminated(inner) => inner,
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
            // TODO: add one to terminated for semicolon
            Expr::Terminated(expr_base) => expr_base.get_span(),
            Expr::NonTerminated(expr_base) => expr_base.get_span(),
        }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Terminated(expr_base) | Expr::NonTerminated(expr_base) => {
                f.write_fmt(format_args!("{expr_base}"))
            }
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    UnexpectedTerminatedExpr(Expr),
    ExpectedTerminatedExpr(Expr),
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
impl Context for ParseError {}

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

// Extras

pub(crate) enum TermState {
    None,
    Term,
    NonTerm,
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
