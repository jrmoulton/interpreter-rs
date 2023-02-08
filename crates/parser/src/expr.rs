use std::fmt::{Display, Write};

use error_stack::Report;
use lexer::{PeekLex, Span, Token};

use crate::{error::*, statement::Statement, structs::ExtendAssign};

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
        function_ident: Box<Expr>,
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
            Expr::FuncCall { function_ident: function, args, .. } => {
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
