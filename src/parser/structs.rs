use crate::lexer::{Lexer, LocTok};
use error_stack::{Context, Report};
use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

#[derive(Debug)]
/// A binary epression has an operator that goes inbetween a lhs operand and a rhs operand.
/// Both the lhs and rhs can be entire expressions themselves
pub(crate) struct BinExp {
    pub(crate) lhs: Box<Expr>,
    pub(crate) operator: LocTok,
    pub(crate) rhs: Box<Expr>,
}

#[derive(Debug)]
/// A prefix expression has an operator before a single operand that can be an entire expression
pub(crate) struct PreExpr {
    pub(crate) operator: LocTok,
    pub(crate) expression: Box<Expr>,
}

#[derive(Debug)]
/// The optional alterantive of an if expression
pub(crate) enum ElseIfExpr {
    ElseIf(Box<Expr>),
    Else(Scope),
}

#[derive(Debug)]
/// A faily self explanatory If expression
pub(crate) struct IfExpr {
    pub(crate) condition: Box<Expr>,
    pub(crate) consequence: Scope,
    pub(crate) alternative: Option<ElseIfExpr>,
}

#[derive(Debug)]
pub(crate) struct Ident(pub(crate) LocTok);

#[derive(Debug)]
// All of the differenct valid cases of expressions
pub(crate) enum Expr {
    IntLiteral(LocTok),
    BoolLiteral(LocTok),
    FuncLiteral(FnLiteral),
    Identifier(Ident),
    PrefixExpression(PreExpr),
    BinaryExpression(BinExp),
    If(IfExpr),
}

#[derive(Debug)]
pub(crate) struct Parameters {}

#[derive(Debug)]
pub(crate) struct FnLiteral {
    pub(crate) parameters: Vec<Ident>,
    pub(crate) body: Scope,
}

#[derive(Debug)]
pub(crate) struct LetStatement {
    pub(crate) ident: LocTok,
    pub(crate) expr: Expr,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(Expr),
    Expression(Expr),
    Scope(Vec<Statement>),
}

#[derive(Debug)]
pub(crate) enum ParseError {
    UnexpectedToken(LocTok),
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
        ParseErrors(vec![parse_error])
    }
}
#[derive(Debug)]
pub(crate) struct ParseErrors(pub(crate) Vec<Report<ParseError>>);
impl Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for ParseErrors {}

#[derive(Debug)]
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
