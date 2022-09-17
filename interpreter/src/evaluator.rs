use std::fmt::Display;

use error_stack::{Context, Report, Result};

use crate::{
    lexer::{LocTok, Token},
    object,
    parser::structs::{self as pstructs, BinExp, ExprBase, PreExpr},
};

mod tests;

#[derive(Debug)]
pub(crate) enum EvalError {
    UnsupportedOperation(ExprBase),
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for EvalError {}

pub(crate) fn eval(
    statements: Vec<pstructs::Statement>,
) -> std::result::Result<object::Object, Vec<Report<EvalError>>> {
    let mut errors = Vec::new();
    let mut last_obj = None;
    for statement in statements {
        let temp_res = match statement {
            pstructs::Statement::Let(_) => unimplemented!(),
            pstructs::Statement::Return(_) => unimplemented!(),
            pstructs::Statement::Expression(expr) => eval_expression(expr),
            pstructs::Statement::Assign(_) => unimplemented!(),
        };
        last_obj = match temp_res {
            Ok(obj) => Some(obj),
            Err(e) => {
                errors.push(e);
                None
            }
        }
    }
    if errors.is_empty() {
        if let Some(obj) = last_obj {
            Ok(obj)
        } else {
            Err(errors)
        }
    } else {
        Err(errors)
    }
}

fn eval_expression(expr: pstructs::Expr) -> Result<object::Object, EvalError> {
    match expr {
        pstructs::Expr::Terminated(_) => unimplemented!(),
        pstructs::Expr::NonTerminated(expr_base) => eval_expr_base(expr_base),
    }
}

fn eval_expr_base(expr_base: ExprBase) -> Result<object::Object, EvalError> {
    match expr_base {
        ExprBase::IntLiteral(LocTok {
            token: Token::Int(int),
            ..
        }) => Ok(object::Object::Integer(object::Integer::new(int))),
        ExprBase::BoolLiteral(LocTok { token, .. }) => match token {
            Token::True => Ok(object::Object::Boolean(object::Boolean::new(true))),
            Token::False => Ok(object::Object::Boolean(object::Boolean::new(false))),
            _ => {
                unreachable!("BoolLiteral will never have a token that is not either true or false")
            }
        },
        ExprBase::FuncLiteral(_) => unimplemented!(),
        ExprBase::CallExpression(_) => unimplemented!(),
        ExprBase::Identifier(_) => unimplemented!(),
        ExprBase::Scope(_) => unimplemented!(),
        ExprBase::PrefixExpression(PreExpr {
            ref operator,
            ref expression,
        }) => {
            let right = eval_expression((**expression).clone())?;
            eval_prefix_expr(&operator.token, right, expr_base.clone())
        }
        ExprBase::BinaryExpression(BinExp {
            ref lhs,
            ref operator,
            ref rhs,
        }) => {
            let left = eval_expr_base((**lhs).clone())?;
            let right = eval_expr_base((**rhs).clone())?;
            eval_binary_expr(&operator.token, left, right, expr_base.clone())
        }
        ExprBase::If(_) => unimplemented!(),
        ExprBase::IntLiteral(_) => unreachable!(
            "Int token matched above. An IntLiteral will never have a token that is not an Int"
        ),
    }
}

#[allow(unreachable_patterns)]
fn eval_binary_expr(
    operator: &Token,
    left: object::Object,
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    match left {
        object::Object::Integer(_) => eval_int_binary_expr(operator, left, right, expr_base),
        object::Object::Boolean(_) => eval_bool_binary_expr(operator, left, right, expr_base),
        _ => unimplemented!(),
    }
}

fn eval_bool_binary_expr(
    operator: &Token,
    left: object::Object,
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    let left = match left {
        object::Object::Boolean(object::Boolean { value }) => value,
        _ => unreachable!("boolean was already matched"),
    };
    let right = match right {
        object::Object::Boolean(object::Boolean { value }) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        Token::Eq => Ok(object::Boolean::new(left == right).into()),
        Token::Ne => Ok(object::Boolean::new(left != right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_int_binary_expr(
    operator: &Token,
    left: object::Object,
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    let left = match left {
        object::Object::Integer(object::Integer { value }) => value,
        _ => unreachable!(),
    };
    let right = match right {
        object::Object::Integer(object::Integer { value }) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        Token::Plus => Ok(object::Integer::new(left + right).into()),
        Token::Minus => Ok(object::Integer::new(left - right).into()),
        Token::Slash => Ok(object::Integer::new(left / right).into()),
        Token::Asterisk => Ok(object::Integer::new(left * right).into()),
        Token::LT => Ok(object::Boolean::new(left < right).into()),
        Token::GT => Ok(object::Boolean::new(left > right).into()),
        Token::Eq => Ok(object::Boolean::new(left == right).into()),
        Token::Ne => Ok(object::Boolean::new(left != right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_prefix_expr(
    operator: &Token,
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    match operator {
        Token::Minus => eval_minux_prefix_op(right, expr_base),
        Token::Bang => eval_bang_prefix_op(right),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_minux_prefix_op(
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    match right {
        object::Object::Integer(object::Integer { value }) => {
            Ok(object::Integer::new(-value).into())
        }
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bang_prefix_op(right: object::Object) -> Result<object::Object, EvalError> {
    match right {
        object::Object::Integer(object::Integer { value }) => {
            Ok(object::Integer::new(!value).into())
        }
        object::Object::Boolean(object::Boolean { value }) => {
            Ok(object::Boolean::new(!value).into())
        } // _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
