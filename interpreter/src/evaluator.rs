use std::fmt::Display;

use error_stack::{Context, Report, Result};

use crate::{
    lexer::{LocTok, Token},
    object,
    parser::structs::{self as pstructs, BinExp, ExprBase, IfExpr, PreExpr},
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

#[derive(Debug)]
pub(crate) struct EvalErrors {
    errors: Vec<Report<EvalError>>,
}
impl From<Report<EvalError>> for EvalErrors {
    fn from(error: Report<EvalError>) -> Self {
        Self {
            errors: vec![error],
        }
    }
}

pub(crate) fn eval(
    statements: Vec<pstructs::Statement>,
) -> std::result::Result<object::Object, EvalErrors> {
    eval_statements(statements, true)
}

fn eval_statements(
    statements: Vec<pstructs::Statement>,
    _catch_return: bool,
) -> std::result::Result<object::Object, EvalErrors> {
    let mut errors = Vec::new();
    let mut last_obj_tup = None;
    for statement in statements {
        let temp_res = match statement {
            pstructs::Statement::Let(_) => unimplemented!(),
            pstructs::Statement::Return(expr) => {
                match expr {
                    Some(expr) => {
                        last_obj_tup = match eval_expression(expr) {
                            Ok(mut obj) => {
                                // This makes it so that even though the expresson is terminated it
                                // will still be passed back out
                                obj.1 = false;
                                Some(obj)
                            }
                            Err(e) => {
                                errors.extend(e.errors);
                                None
                            }
                        };
                    }
                    None => {
                        last_obj_tup = None;
                    }
                }
                break;
            }
            pstructs::Statement::Expression(expr) => eval_expression(expr),
            pstructs::Statement::Assign(_) => unimplemented!(),
        };
        last_obj_tup = match temp_res {
            Ok(obj) => Some(obj),
            Err(e) => {
                errors.extend(e.errors);
                None
            }
        }
    }
    if errors.is_empty() {
        if let Some(obj) = last_obj_tup {
            if !obj.1 {
                Ok(obj.0)
            } else {
                Ok(object::Object::Empty(object::Empty::new(())))
            }
        } else {
            Err(EvalErrors { errors })
        }
    } else {
        Err(EvalErrors { errors })
    }
}

fn eval_expression(
    expr: pstructs::Expr,
) -> std::result::Result<(object::Object, bool), EvalErrors> {
    match expr {
        pstructs::Expr::Terminated(expr_base) => Ok((eval_expr_base(expr_base)?, true)),
        pstructs::Expr::NonTerminated(expr_base) => Ok((eval_expr_base(expr_base)?, false)),
    }
}

fn eval_expr_base(expr_base: ExprBase) -> std::result::Result<object::Object, EvalErrors> {
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
            let right = eval_expression((**expression).clone())?.0;
            Ok(eval_prefix_expr(&operator.token, right, expr_base.clone())?)
        }
        ExprBase::BinaryExpression(BinExp {
            ref lhs,
            ref operator,
            ref rhs,
        }) => {
            let left = eval_expr_base((**lhs).clone())?;
            let right = eval_expr_base((**rhs).clone())?;
            Ok(eval_binary_expr(
                &operator.token,
                left,
                right,
                expr_base.clone(),
            )?)
        }
        ExprBase::If(IfExpr {
            ref condition,
            consequence,
            alternative,
        }) => eval_if_expr(condition, consequence, alternative),
        ExprBase::IntLiteral(_) => unreachable!(
            "Int token matched above. An IntLiteral will never have a token that is not an Int"
        ),
    }
}

fn eval_if_expr(
    condition: &ExprBase,
    consequence: pstructs::Scope,
    alternative: Option<pstructs::ElseIfExpr>,
) -> std::result::Result<object::Object, EvalErrors> {
    let cond_bool = eval_expr_base((*condition).clone())?;
    match cond_bool {
        object::Object::Boolean(object::Boolean { value }) => {
            if value {
                eval_statements(consequence.statements, false)
            } else if let Some(alt) = alternative {
                match alt {
                    pstructs::ElseIfExpr::ElseIf(else_expr_base) => eval_expr_base(*else_expr_base),
                    pstructs::ElseIfExpr::Else(if_expr) => {
                        eval_statements(if_expr.statements, false)
                    }
                }
            } else {
                Ok(object::Object::Empty(object::Empty::new(())))
            }
        }
        _ => unreachable!(),
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
        Token::Bang => eval_bang_prefix_op(right, expr_base),
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

fn eval_bang_prefix_op(
    right: object::Object,
    expr_base: ExprBase,
) -> Result<object::Object, EvalError> {
    match right {
        object::Object::Integer(object::Integer { value }) => {
            Ok(object::Integer::new(!value).into())
        }
        object::Object::Boolean(object::Boolean { value }) => {
            Ok(object::Boolean::new(!value).into())
        }
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
