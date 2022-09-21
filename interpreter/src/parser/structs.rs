use crate::lexer::{Lexer, LocTok};
use error_stack::{Context, Report};
use std::fmt::Write;
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
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

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
    StringLiteral(LocTok),
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
            ExprBase::IntLiteral(lok_tok)
            | ExprBase::BoolLiteral(lok_tok)
            | ExprBase::StringLiteral(lok_tok) => {
                format!("{lok_tok}",)
            }
            ExprBase::FuncLiteral(_fn_literal) => todo!(),
            ExprBase::CallExpression(CallExpr { function, args }) => {
                let mut ret_str = String::from("[ ");
                for arg in args.iter() {
                    write!(ret_str, "{arg}, ")?;
                }
                ret_str.push_str(" ]");
                format!("FunctionCall: func{{{function}}}, args{{{ret_str}}}")
            }
            ExprBase::Identifier(ident) => format!("{ident}"),
            ExprBase::Scope(_statements) => todo!(),
            ExprBase::PrefixExpression(PreExpr {
                operator,
                expression,
            }) => {
                format!("{operator}:{}", expression)
            }
            ExprBase::BinaryExpression(bin_expr) => {
                format!("({} {} {})", bin_expr.lhs, bin_expr.operator, bin_expr.rhs)
            }
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
            ExprBase::StringLiteral(_) => "String".into(),
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
impl Display for AssignStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.ident, self.expr))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LetStatement {
    pub(crate) ident: LocTok,
    pub(crate) expr: Expr,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("let {} = {}", self.ident, self.expr))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(Option<Expr>),
    Expression(Expr),
    Assign(AssignStatement),
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temp = match self {
            Statement::Let(let_statement) => format!("{let_statement}"),
            Statement::Return(return_statement) => match return_statement {
                Some(expr) => format!("return {expr};"),
                None => "return;".into(),
            },
            Statement::Expression(expression_statement) => format!("{expression_statement}"),
            Statement::Assign(assign_statement) => format!("{assign_statement}"),
        };
        f.write_str(&temp)
    }
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
        let temp = match self {
            ParseError::UnexpectedToken(lok_tok) => format!("Unexpected Token: {}", lok_tok.token),
            ParseError::UnexpectedTerminatedExpr(expr) => {
                format!("Unexpected token `;` after expression: {expr}")
            }
            ParseError::ExpectedTerminatedExpr(expr) => {
                format!("Expected ';' after expression: {expr}")
            }
            ParseError::Eof => "Unexpected end of input".into(),
        };
        f.write_str(&temp)
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
        let mut ret_str = String::from("Parse Errors[ ");
        for error in self.errors.iter() {
            write!(ret_str, "{error}")?;
        }
        ret_str.push_str(" ]");
        f.write_str(&ret_str)
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
