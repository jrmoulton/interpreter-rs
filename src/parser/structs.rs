use crate::lexer::{Lexer, LocTok};
use error_stack::{Context, Report};
use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

#[derive(Debug)]
/// A binary epression has an operator that goes inbetween a lhs operand and a rhs operand.
/// Both the lhs and rhs can be entire expressions themselves
pub(crate) struct BinExp {
    pub(crate) lhs: Box<ExprBase>,
    pub(crate) operator: LocTok,
    pub(crate) rhs: Box<ExprBase>,
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
    ElseIf(Box<ExprBase>),
    Else(Scope),
}

#[derive(Debug)]
pub(crate) struct CallExpr {
    pub(crate) function: Box<ExprBase>,
    pub(crate) args: Vec<Expr>,
}

#[derive(Debug)]
/// A faily self explanatory If expression
pub(crate) struct IfExpr {
    pub(crate) condition: Box<ExprBase>,
    pub(crate) consequence: Scope,
    pub(crate) alternative: Option<ElseIfExpr>,
}

#[derive(Debug)]
pub(crate) struct Ident(pub(crate) LocTok);

#[derive(Debug)]
// All of the differenct valid cases of expressions
pub(crate) enum Expr {
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
}

#[derive(Debug)]
pub(crate) enum ExprBase {
    IntLiteral(LocTok),
    BoolLiteral(LocTok),
    FuncLiteral(FnLiteral),
    CallExpression(CallExpr),
    Identifier(Ident),
    Assign(AssignExpr),
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
pub(crate) struct AssignExpr {
    pub(crate) ident: LocTok,
    pub(crate) expr: Box<ExprBase>,
}

#[derive(Debug)]
pub(crate) struct LetStatement {
    pub(crate) ident: LocTok,
    pub(crate) expr: Expr,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(Option<Expr>),
    Expression(Expr),
    Scope(Vec<Statement>),
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

pub(crate) trait YeetErrToVec {
    type Target;
    fn yeet_to_vec(self, vec: &mut Vec<Report<ParseError>>) -> Option<Self::Target>;
}
// impl YeetErrToVec for Result<ExprBase, ParseErrors> {
//     type Target = ExprBase;
//     fn yeet_to_vec(self, vec: &mut Vec<Report<ParseError>>) -> Option<Self::Target> {
//         match self {
//             Err(errs) => {
//                 vec.extend(errs.0);
//                 None
//             }
//             Ok(val) => Some(val),
//         }
//     }
// }
