use crate::lexer::{Lexer, LocTok};
use error_stack::{Context, Report};
use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

#[derive(Debug, Clone)]
/// A binary epression has an operator that goes inbetween a lhs operand and a rhs operand.
/// Both the lhs and rhs can be entire expressions themselves
pub(crate) struct BinExp {
    pub(crate) lhs: Box<ExprBase>,
    pub(crate) operator: LocTok,
    pub(crate) rhs: Box<ExprBase>,
}

#[derive(Debug, Clone)]
/// A prefix expression has an operator before a single operand that can be an entire expression
pub(crate) struct PreExpr {
    pub(crate) operator: LocTok,
    pub(crate) expression: Box<Expr>,
}

#[derive(Debug, Clone)]
/// The optional alterantive of an if expression
pub(crate) enum ElseIfExpr {
    ElseIf(Box<ExprBase>),
    Else(Scope),
}

#[derive(Debug, Clone)]
pub(crate) struct CallExpr {
    pub(crate) function: Box<ExprBase>,
    pub(crate) args: Vec<Expr>,
}

#[derive(Debug, Clone)]
/// A faily self explanatory If expression
pub(crate) struct IfExpr {
    pub(crate) condition: Box<ExprBase>,
    pub(crate) consequence: Scope,
    pub(crate) alternative: Option<ElseIfExpr>,
}

#[derive(Debug, Clone)]
pub(crate) struct Ident(pub(crate) LocTok);

#[derive(Debug, Clone)]
// All of the differenct valid cases of expressions
pub(crate) enum Expr {
    Terminated(ExprBase),
    NonTerminated(ExprBase),
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
impl Expr {
    fn type_string(&self) -> String {
        match self {
            Expr::Terminated(expr_base) | Expr::NonTerminated(expr_base) => expr_base.type_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ExprBase {
    IntLiteral(LocTok),
    BoolLiteral(LocTok),
    FuncLiteral(FnLiteral),
    CallExpression(CallExpr),
    Identifier(Ident),
    Scope(Vec<Statement>),
    PrefixExpression(PreExpr),
    BinaryExpression(BinExp),
    If(IfExpr),
}
impl Display for ExprBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret_str = match self {
            ExprBase::IntLiteral(lok_tok) | ExprBase::BoolLiteral(lok_tok) => {
                format!(
                    "{lok_tok} at line:{} column:{}",
                    lok_tok.line + 1,
                    lok_tok.column + 1
                )
            }
            ExprBase::FuncLiteral(_fn_literal) => todo!(),
            ExprBase::CallExpression(_call_expr) => todo!(),
            ExprBase::Identifier(_ident) => todo!(),
            ExprBase::Scope(_statements) => todo!(),
            ExprBase::PrefixExpression(PreExpr {
                operator,
                expression,
            }) => {
                format!(
                    "Op: {operator} for {}: {}",
                    expression.type_string(),
                    expression
                )
            }
            ExprBase::BinaryExpression(_bin_expr) => todo!(),
            ExprBase::If(IfExpr { condition, .. }) => {
                format!("{condition}")
            }
        };
        f.write_str(&ret_str)
    }
}
impl ExprBase {
    fn type_string(&self) -> String {
        match self {
            ExprBase::IntLiteral(_) => "Integer".into(),
            ExprBase::BoolLiteral(_) => "Boolean".into(),
            ExprBase::FuncLiteral(_) => "Function".into(),
            ExprBase::CallExpression(_) => "Function Call".into(),
            ExprBase::Identifier(_) => "Identifier".into(),
            ExprBase::Scope(_) => "Scope".into(),
            ExprBase::PrefixExpression(_) => "Prefix Expression".into(),
            ExprBase::BinaryExpression(_) => "Binary Expression".into(),
            ExprBase::If(_) => "If Expression".into(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Parameters {}

#[derive(Debug, Clone)]
pub(crate) struct FnLiteral {
    pub(crate) parameters: Vec<Ident>,
    pub(crate) body: Scope,
}

#[derive(Debug, Clone)]
pub(crate) struct AssignStatement {
    pub(crate) ident: LocTok,
    pub(crate) expr: Box<ExprBase>,
}

#[derive(Debug, Clone)]
pub(crate) struct LetStatement {
    pub(crate) ident: LocTok,
    pub(crate) expr: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(Option<Expr>),
    Expression(Expr),
    Assign(AssignStatement),
}

#[derive(Debug)]
pub(crate) enum ParseError {
    UnexpectedToken(LocTok),
    UnexpectedTerminatedExpr(Expr),
    ExpectedTerminatedExpr(Expr),
    Eof,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for ParseError {}
impl From<Report<ParseError>> for ParseErrors {
    fn from(parse_error: Report<ParseError>) -> Self {
        ParseErrors {
            errors: vec![parse_error],
        }
    }
}
#[derive(Debug)]
pub(crate) struct ParseErrors {
    pub(crate) errors: Vec<Report<ParseError>>,
}
impl Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for ParseErrors {}

pub(crate) struct Suggestion(pub &'static str);

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) statements: Vec<Statement>,
}
impl Scope {
    pub(crate) fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}
impl IntoIterator for Scope {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Statement>;
    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

pub(crate) type LexerPeekRef<'a> = Rc<RefCell<Peekable<Lexer<'a>>>>;
