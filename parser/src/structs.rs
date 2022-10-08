use error_stack::Context;
use lexer::{Lexer, Span, Token};
use std::fmt::Write;
use std::{fmt::Display, iter::Peekable};

pub(crate) type PeekLex<'a> = Peekable<Lexer<'a>>;

#[derive(Debug, Clone)]
/// A binary epression has an operator that goes inbetween a lhs operand and a rhs operand.
/// Both the lhs and rhs can be entire expressions themselves
pub struct BinExp {
    pub lhs: Box<ExprBase>,
    pub operator: Token,
    pub rhs: Box<ExprBase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
/// A prefix expression has an operator before a single operand that can be an entire expression
pub struct PreExpr {
    pub operator: Token,
    pub expression: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
/// The optional alterantive of an if expression
pub enum ElseIfExpr {
    ElseIf(Box<ExprBase>),
    Else(Scope),
}
impl ElseIfExpr {
    pub fn get_span(&self) -> Span {
        match self {
            ElseIfExpr::ElseIf(expr_base) => expr_base.get_span(),
            ElseIfExpr::Else(scope) => scope.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Box<ExprBase>,
    pub args: Vec<ExprBase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethCall {
    /// An expression that can resolve to a value
    pub instance: Box<ExprBase>,
    /// an call expression that with the method ident comes afer the Dot
    pub method: Box<ExprBase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
/// A faily self explanatory If expression
pub struct IfExpr {
    pub condition: Box<ExprBase>,
    pub consequence: Scope,
    pub alternative: Option<ElseIfExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident(pub Token);
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
    FuncLiteral(FnLiteral),
    StringLiteral(Token),
    Array(Array),
    CallExpression(CallExpr),
    Identifier(Ident),
    Scope(Scope),
    PrefixExpression(PreExpr),
    BinaryExpression(BinExp),
    IndexExpression(IndExpr),
    MethodCall(MethCall),
    If(IfExpr),
}
impl ExprBase {
    pub fn get_span(&self) -> Span {
        match self {
            ExprBase::IntLiteral(token) => token.span,
            ExprBase::BoolLiteral(token) => token.span,
            ExprBase::FuncLiteral(fn_literal) => fn_literal.span,
            ExprBase::StringLiteral(token) => token.span,
            ExprBase::Array(array) => array.span,
            ExprBase::CallExpression(call_expr) => call_expr.span,
            ExprBase::Identifier(ident) => ident.0.span,
            ExprBase::Scope(scope) => scope.span,
            ExprBase::PrefixExpression(pre_expr) => pre_expr.span,
            ExprBase::BinaryExpression(bin_exp) => bin_exp.span,
            ExprBase::IndexExpression(ind_exp) => ind_exp.span,
            ExprBase::MethodCall(meth_call) => meth_call.span,
            ExprBase::If(if_expr) => if_expr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub exprs: Vec<ExprBase>,
    span: Span,
}
impl Array {
    pub fn new(expressions: Vec<ExprBase>, span: Span) -> Self {
        Self {
            exprs: expressions,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IndExpr {
    pub array: Box<ExprBase>,
    pub index: Box<ExprBase>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnLiteral {
    pub parameters: Vec<Ident>,
    pub body: Scope,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssignStatement {
    pub ident: Token,
    pub expr: Box<ExprBase>,
    pub span: Span,
}
impl Display for AssignStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.ident, self.expr))
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub ident: Token,
    pub expr: Expr,
    pub span: Span,
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
    UnexpectedToken(Token),
    UnexpectedTerminatedExpr(Expr),
    ExpectedTerminatedExpr(Expr),
    MultipleUnterminatedExpressions(Expr),
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
    pub span: Span,
}
impl Scope {
    pub(crate) fn new(statements: Vec<Statement>, span: Span) -> Self {
        Self { statements, span }
    }
}
impl IntoIterator for Scope {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Statement>;
    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
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
            ExprBase::Array(items) => {
                let mut ret_str = String::from("[ ");
                for item in items.exprs.iter() {
                    write!(ret_str, "{item}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
            ExprBase::IndexExpression(IndExpr { array, index, .. }) => {
                format!("{array}[{index}]")
            }
            ExprBase::MethodCall(MethCall {
                instance, method, ..
            }) => {
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
            ExprBase::CallExpression(CallExpr { function, args, .. }) => {
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
                for statement in statements.statements.iter() {
                    write!(ret_str, "{statement}, ")?;
                }
                ret_str.push_str(" ]");
                ret_str
            }
            ExprBase::PrefixExpression(PreExpr {
                operator,
                expression,
                ..
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
