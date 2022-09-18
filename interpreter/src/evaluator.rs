use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use error_stack::{Context, Report, Result};

use crate::{
    lexer::{LocTok, Token},
    object::{self, ObjectTrait},
    parser::structs::{
        self as pstructs, AssignStatement, BinExp, ExprBase, Ident, IfExpr, PreExpr,
    },
};

mod tests;

#[derive(Debug)]
pub(crate) enum EvalError {
    UnsupportedOperation(ExprBase),
    IdentifierNotFound,
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

pub(crate) type Environment = Rc<RefCell<HashMap<String, object::Object>>>;

pub(crate) fn eval(
    statements: Vec<pstructs::Statement>,
    env: Environment,
) -> std::result::Result<object::Object, EvalErrors> {
    let mut errors = Vec::new();
    let mut last_obj_tup = None;
    for statement in statements {
        let temp_res = match statement {
            pstructs::Statement::Let(let_statement) => {
                let expr_obj = eval_expression(let_statement.expr, env.clone())?.0;
                let ident_string = match let_statement.ident.token {
                    Token::Ident(inner) => inner,
                    _ => unreachable!(),
                };
                env.borrow_mut().insert(ident_string, expr_obj);
                Ok((object::Empty::new(()).into(), true))
            }
            pstructs::Statement::Return(expr) => {
                match expr {
                    Some(expr) => {
                        last_obj_tup = match eval_expression(expr, env.clone()) {
                            Ok(mut obj) => {
                                // This makes it so that even though the expresson is terminated it
                                // will still be passed back out
                                obj.0.set_return();
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
            pstructs::Statement::Expression(expr) => eval_expression(expr, env.clone()),
            pstructs::Statement::Assign(AssignStatement {
                ident:
                    LocTok {
                        token: Token::Ident(ident_string),
                        ..
                    },
                expr,
            }) => {
                let key_exists = env.borrow_mut().contains_key(&ident_string);
                let expr_obj = eval_expr_base(*expr, env.clone())?;
                if key_exists {
                    env.borrow_mut().insert(ident_string, expr_obj);
                } else {
                    Err(Report::new(EvalError::IdentifierNotFound))?
                }
                Ok((object::Empty::new(()).into(), true))
            }
            pstructs::Statement::Assign(_) => unreachable!("Assign statement matched above"),
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
            if !obj.1 || obj.0.is_return() {
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
    env: Environment,
) -> std::result::Result<(object::Object, bool), EvalErrors> {
    match expr {
        pstructs::Expr::Terminated(expr_base) => {
            Ok((eval_expr_base(expr_base, env.clone())?, true))
        }
        pstructs::Expr::NonTerminated(expr_base) => {
            Ok((eval_expr_base(expr_base, env.clone())?, false))
        }
    }
}

fn eval_expr_base(
    expr_base: ExprBase,
    env: Environment,
) -> std::result::Result<object::Object, EvalErrors> {
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
        ExprBase::Identifier(Ident(LocTok {
            token: Token::Ident(ident_string),
            ..
        })) => {
                match env.borrow_mut().get(&ident_string) {
                    Some(obj) => Ok(obj.clone()),
                    None => Err(Report::new(EvalError::IdentifierNotFound))?
                }
        }
        ExprBase::Scope(_) => unimplemented!(),
        ExprBase::PrefixExpression(PreExpr {
            ref operator,
            ref expression,
        }) => {
            let right = eval_expression((**expression).clone(), env.clone())?.0;
            Ok(eval_prefix_expr(&operator.token, right, expr_base.clone())?)
        }
        ExprBase::BinaryExpression(BinExp {
            ref lhs,
            ref operator,
            ref rhs,
        }) => {
            let left = eval_expr_base((**lhs).clone(), env.clone())?;
            let right = eval_expr_base((**rhs).clone(), env.clone())?;
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
        }) => eval_if_expr(condition, consequence, alternative, env.clone()),
        ExprBase::IntLiteral(_) => unreachable!(
            "Int token matched above. An IntLiteral will never have a token that is not an Int"
        ),
        ExprBase::Identifier(_) => unreachable!(
            "Identifier token matched above. An Identifier will never have a token that is not an Ident"
        ),
    }
}

fn eval_if_expr(
    condition: &ExprBase,
    consequence: pstructs::Scope,
    alternative: Option<pstructs::ElseIfExpr>,
    env: Environment,
) -> std::result::Result<object::Object, EvalErrors> {
    let cond_bool = eval_expr_base((*condition).clone(), env.clone())?;
    match cond_bool {
        object::Object::Boolean(object::Boolean { value, .. }) => {
            if value {
                eval(consequence.statements, env.clone())
            } else if let Some(alt) = alternative {
                match alt {
                    pstructs::ElseIfExpr::ElseIf(else_expr_base) => {
                        eval_expr_base(*else_expr_base, env.clone())
                    }
                    pstructs::ElseIfExpr::Else(if_expr) => eval(if_expr.statements, env.clone()),
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
        object::Object::Boolean(object::Boolean { value, .. }) => value,
        _ => unreachable!("boolean was already matched"),
    };
    let right = match right {
        object::Object::Boolean(object::Boolean { value, .. }) => value,
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
        object::Object::Integer(object::Integer { value, .. }) => value,
        _ => unreachable!(),
    };
    let right = match right {
        object::Object::Integer(object::Integer { value, .. }) => value,
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
        object::Object::Integer(object::Integer { value, .. }) => {
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
        object::Object::Integer(object::Integer { value, .. }) => {
            Ok(object::Integer::new(!value).into())
        }
        object::Object::Boolean(object::Boolean { value, .. }) => {
            Ok(object::Boolean::new(!value).into())
        }
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
