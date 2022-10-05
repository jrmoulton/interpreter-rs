use error_stack::Context;
use lexer::{Lexer, LocTok};
use std::fmt::Write;
use std::{fmt::Display, iter::Peekable};

#[derive(Debug, Clone)]
/// A binary epression has an operator that goes inbetween a lhs operand and a rhs operand.
/// Both the lhs and rhs can be entire expressions themselves
pub struct BinExp {
    pub lhs: Box<ExprBase>,
    pub operator: LocTok,
    pub rhs: Box<ExprBase>,
}

#[derive(Debug, Clone)]
/// A prefix expression has an operator before a single operand that can be an entire expression
pub struct PreExpr {
    pub operator: LocTok,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
/// The optional alterantive of an if expression
pub enum ElseIfExpr {
    ElseIf(Box<ExprBase>),
    Else(Scope),
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Box<ExprBase>,
    pub args: Vec<ExprBase>,
}

#[derive(Debug, Clone)]
pub struct MethCall {
    /// An expression that can resolve to a value
    pub instance: Box<ExprBase>,
    /// an call expression that with the method ident comes afer the Dot
    pub method: Box<ExprBase>,
}

#[derive(Debug, Clone)]
/// A faily self explanatory If expression
pub struct IfExpr {
    pub condition: Box<ExprBase>,
    pub consequence: Scope,
    pub alternative: Option<ElseIfExpr>,
}

#[derive(Debug, Clone)]
pub struct Ident(pub LocTok);
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Debug, Clone)]
// All of the differenct valid cases of expressions
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
    IntLiteral(LocTok),
    BoolLiteral(LocTok),
    FuncLiteral(FnLiteral),
    StringLiteral(LocTok),
    Array(Vec<ExprBase>),
    CallExpression(CallExpr),
    Identifier(Ident),
    Scope(Vec<Statement>),
    PrefixExpression(PreExpr),
    BinaryExpression(BinExp),
    IndexExpression(IndExpr),
    MethodCall(MethCall),
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
            ExprBase::Array(items) => {
                let mut ret_str = String::from("[ ");
                for item in items.iter() {
                    write!(ret_str, "{item}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
            ExprBase::IndexExpression(IndExpr { array, index }) => {
                format!("{array}[{index}]")
            }
            ExprBase::MethodCall(MethCall { instance, method }) => {
                format!("{instance}.{method}")
            }
            ExprBase::FuncLiteral(FnLiteral { parameters, .. }) => {
                let mut ret_str = String::from("(");
                for param in parameters.iter() {
                    write!(ret_str, "{param}, ")?;
                }
                ret_str.push(')');
                format!("fn{ret_str}{{...}}")
            }
            ExprBase::CallExpression(CallExpr { function, args }) => {
                let mut ret_str = String::from("[ ");
                for arg in args.iter() {
                    write!(ret_str, "{arg}, ")?;
                }
                ret_str.push_str(" ]");
                format!("Call: func{{{function}}}, args{{{ret_str}}}")
            }
            ExprBase::Identifier(ident) => format!("{ident}"),
            ExprBase::Scope(statements) => {
                let mut ret_str = String::from("[ ");
                for statement in statements.iter() {
                    write!(ret_str, "{statement}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
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

#[derive(Debug, Clone)]
pub struct IndExpr {
    pub array: Box<ExprBase>,
    pub index: Box<ExprBase>,
}

#[derive(Debug, Clone)]
pub struct FnLiteral {
    pub parameters: Vec<Ident>,
    pub body: Scope,
}

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub ident: LocTok,
    pub expr: Box<ExprBase>,
}
impl Display for AssignStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.ident, self.expr))
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub ident: LocTok,
    pub expr: Expr,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("let {} = {}", self.ident, self.expr))
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
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
pub enum ParseError {
    UnexpectedToken(LocTok),
    UnexpectedTerminatedExpr(Expr),
    ExpectedTerminatedExpr(Expr),
    MultipleUnterminatedExpressions(Expr),
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
            ParseError::MultipleUnterminatedExpressions(expr) => {
                format!("Expected ';' before expression: {expr}")
            }
            ParseError::Eof => "Unexpected end of input".into(),
        };
        f.write_str(&temp)
    }
}
impl Context for ParseError {}

pub struct Suggestion(pub &'static str);

#[derive(Debug, Clone)]
pub struct Scope {
    pub statements: Vec<Statement>,
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

pub(crate) type PeekLex<'a> = Peekable<Lexer<'a>>;
