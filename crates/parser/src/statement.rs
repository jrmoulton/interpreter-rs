use std::fmt::Display;

use lexer::Span;

use crate::expr::Expr;

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
