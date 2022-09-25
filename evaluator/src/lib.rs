mod tests;
pub mod structs;
pub mod object;

use std::{collections::HashMap,  sync::Arc};

use error_stack::{ Report, Result};

use lexer::{LocTok, Token};
use object::{Array, Integer};
use parser::structs::*;
use structs::*;

use crate::object::{FuncIntern, Object, ObjectTrait};



pub fn eval(
    statements: Vec<Statement>,
    env: Arc<Environment>,
) -> std::result::Result<Object, EvalErrors> {
    let mut errors = Vec::new();
    let mut last_obj_tup = None;
    for statement in statements {
        let temp_res = match statement {
            Statement::Let(let_statement) => {
                let expr_obj = eval_expression(let_statement.expr, env.clone())?.0;
                let ident_string = match let_statement.ident.token {
                    Token::Ident(inner) => inner,
                    _ => unreachable!(),
                };
                env.set(ident_string, expr_obj);
                Ok((object::Empty::new(().into()).into(), true))
            }
            Statement::Return(expr) => {
                match expr {
                    Some(expr) => {
                        last_obj_tup = match eval_expression(expr, env) {
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
            Statement::Expression(expr) => eval_expression(expr, env.clone()),
            Statement::Assign(AssignStatement {
                ident,
                expr,
            }) => {
                let ident_string = match ident {
                    LocTok {
                        token: Token::Ident(ref ident_string),
                        ..
                    } => ident_string,
                    _ => unreachable!()
                    };
                let key_exists = env.has(&ident_string.to_string());
                let expr_obj = eval_expr_base(*expr, env.clone())?;
                if key_exists {
                    env.set(ident_string.into(), expr_obj);
                } else {
                    Err(Report::new(EvalError::IdentifierNotFound(ident_string.clone())).attach(ident))?
                }
                Ok((object::Empty::new(().into()).into(), true))
            }
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
                Ok(Object::Empty(object::Empty::new(().into())))
            }
        } else {
            Err(EvalErrors { errors })
        }
    } else {
        Err(EvalErrors { errors })
    }
}

fn eval_expression(
    expr: Expr,
    env: Arc<Environment>,
) -> std::result::Result<(Object, bool), EvalErrors> {
    match expr {
        Expr::Terminated(expr_base) => Ok((eval_expr_base(expr_base, env)?, true)),
        Expr::NonTerminated(expr_base) => Ok((eval_expr_base(expr_base, env)?, false)),
    }
}

fn eval_expr_base(
    expr_base: ExprBase,
    env: Arc<Environment>,
) -> std::result::Result<Object, EvalErrors> {
    match expr_base {
        ExprBase::IntLiteral(LocTok {
            token: Token::Int(int),
            ..
        }) => Ok(Object::Integer(object::Integer::new(int))),
        ExprBase::BoolLiteral(LocTok { token, .. }) => match token {
            Token::True => Ok(Object::Boolean(object::Boolean::new(true))),
            Token::False => Ok(Object::Boolean(object::Boolean::new(false))),
            _ => {
                unreachable!("BoolLiteral will never have a token that is not either true or false")
            }
        }, 
        ExprBase::StringLiteral(LocTok {token: Token::String(inner_str), ..}) => Ok(Object::String(object::String::new(inner_str))),
        ExprBase::FuncLiteral(FnLiteral { parameters, body }) => Ok(Object::Function(object::Function::new(FuncIntern::new(parameters, body, env)))),
        ExprBase::CallExpression(CallExpr { function, args }) => {
            let function_obj = eval_expr_base(*function, env.clone())?;
            let args: std::result::Result<Vec<Object>, EvalErrors> = args.iter().map(|arg| eval_expr_base(arg.clone(), env.clone())).collect();
            Ok(apply_function(function_obj, args?)?)
        },
        ExprBase::MethodCall(MethCall { instance, method }) => {
            let _value = eval_expr_base(*instance, env.clone())?;
            let _value_type = _value.type_string();
            let _method_call = eval_expr_base(*method, env)?;
            // let env.find(&value_type).expect("Exists");
            unimplemented!()
        },
        ExprBase::Array(items) => {
            let items: std::result::Result<Vec<Object>, EvalErrors> = items.iter().map(|item| eval_expr_base(item.clone(), env.clone())).collect();
            Ok(Object::Array(object::Array::new(items?.into())))
        }
        ExprBase::IndexExpression(IndExpr { array, index }) => {
            let array_obj = eval_expr_base(*array, env.clone())?;
            let index = eval_expr_base(*index, env)?;
            let index = match index {
                Object::Integer(Integer { value, .. }) => value,
                _ =>  Err(Report::new(EvalError::UnexpectedObject(index)))?

            };
            match array_obj {
                Object::Array(Array { value, .. } ) => {
                    Ok(value[index as usize].clone())
                },
                _ =>  Err(Report::new(EvalError::UnexpectedObject(array_obj)))?
            } 
        }
        ExprBase::Identifier(Ident(LocTok {
            token: Token::Ident(ref ident_string),
            ..
        })) => {
                match env.find(ident_string) {
                    Some(obj) => Ok(obj),
                    None => Err(Report::new(EvalError::IdentifierNotFound(ident_string.clone())).attach(expr_base))?
                }
        }
        ExprBase::Scope(_) => unimplemented!(),
        ExprBase::PrefixExpression(PreExpr {
            ref operator,
            ref expression,
        }) => {
            let right = eval_expression((**expression).clone(), env)?.0;
            Ok(eval_prefix_expr(&operator.token, right, expr_base.clone())?)
        } 
        ExprBase::BinaryExpression(BinExp {
            ref lhs,
            ref operator,
            ref rhs,
        }) => {
            let left = eval_expr_base((**lhs).clone(), env.clone())?;
            let right = eval_expr_base((**rhs).clone(), env)?;
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
        }) => eval_if_expr(condition, consequence, alternative, env),
        ExprBase::IntLiteral(_) => unreachable!(
            "Int token matched above. An IntLiteral will never have a token that is not an Int"
        ),
        ExprBase::Identifier(_) => unreachable!(
            "Identifier token matched above. An Identifier will never have a token that is not an Ident"
        ),
        ExprBase::StringLiteral(_) => unreachable!(
            "String token matched above. An String will never have a token that is not an String"
        ),
    }
}

fn apply_function(function_obj: Object, args: Vec<Object>) -> std::result::Result<Object, EvalErrors> {
    match function_obj {
        Object::Function(function_obj) => {
            let fn_literal = function_obj.value;
            let param_strings: Vec<String> = fn_literal.parameters.iter().map(|param_ident| {match &param_ident.0.token {
                Token::Ident(ident_string) => ident_string.clone(),
                _ => unreachable!()
            }}).collect();
            if param_strings.len() != args.len() {
                Err(Report::new(EvalError::MismatchedNumOfFunctionParams))?
            }
            let mut new_env = HashMap::new();
            param_strings.iter().zip(args).for_each(|(param, object)| {new_env.insert(param.clone(), object);});
            eval(fn_literal.body.statements, Arc::new(Environment::new_from_map_and_outer(new_env, fn_literal.env)))
        },
        _ => unreachable!("No other objects match to calling this function {function_obj}")
    }
}

fn eval_if_expr(
    condition: &ExprBase,
    consequence: Scope,
    alternative: Option<ElseIfExpr>,
    env: Arc<Environment>,
) -> std::result::Result<Object, EvalErrors> {
    let cond_bool = eval_expr_base((*condition).clone(), env.clone())?;
    match cond_bool {
        Object::Boolean(object::Boolean { value, .. }) => {
            if value {
                eval(consequence.statements, env)
            } else if let Some(alt) = alternative {
                match alt {
                    ElseIfExpr::ElseIf(else_expr_base) => {
                        eval_expr_base(*else_expr_base, env)
                    }
                    ElseIfExpr::Else(if_expr) => eval(if_expr.statements, env),
                }
            } else {
                Ok(Object::Empty(object::Empty::new(().into())))
            }
        }
        _ => Err(Report::new(EvalError::InvalidIfCondition(condition.clone())))?,
    }
}

#[allow(unreachable_patterns)]
fn eval_binary_expr(
    operator: &Token,
    left: Object,
    right: Object,
    expr_base: ExprBase,
) -> Result<Object, EvalError> {
    match left {
        Object::Integer(_) => eval_int_binary_expr(operator, left, right, expr_base),
        Object::Boolean(_) => eval_bool_binary_expr(operator, left, right, expr_base),
        Object::String(_) => eval_string_binary_expr(operator, left, right, expr_base),
        _ => unimplemented!(),
    }
}

fn eval_string_binary_expr(operator: &Token, left: Object, right: Object, expr_base: ExprBase) -> Result<Object, EvalError> {
    let left = match left {
        Object::String(object::String { value, .. }) => value,
        _ => unreachable!("string was already matched"),
    };
    let right = match right {
        Object::String(object::String { value, .. }) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        Token::Eq => Ok(object::Boolean::new(left == right).into()),
        Token::Ne => Ok(object::Boolean::new(left != right).into()),
        Token::Plus => Ok(object::String::new(left + &right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
    
}

fn eval_bool_binary_expr(
    operator: &Token,
    left: Object,
    right: Object,
    expr_base: ExprBase,
) -> Result<Object, EvalError> {
    let left = match left {
        Object::Boolean(object::Boolean { value, .. }) => value,
        _ => unreachable!("boolean was already matched"),
    };
    let right = match right {
        Object::Boolean(object::Boolean { value, .. }) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        Token::Eq => Ok(object::Boolean::new(left == right).into()),
        Token::Ne => Ok(object::Boolean::new(left != right).into()),
        Token::Or => Ok(object::Boolean::new(left || right).into()),
        Token::And => Ok(object::Boolean::new(left && right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_int_binary_expr(
    operator: &Token,
    left: Object,
    right: Object,
    expr_base: ExprBase,
) -> Result<Object, EvalError> {
    let left = match left {
        Object::Integer(object::Integer { value, .. }) => value,
        _ => unreachable!(),
    };
    let right = match right {
        Object::Integer(object::Integer { value, .. }) => value,
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
    right: Object,
    expr_base: ExprBase,
) -> Result<Object, EvalError> {
    match operator {
        Token::Minus => eval_minux_prefix_op(right, expr_base),
        Token::Bang => eval_bang_prefix_op(right, expr_base),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_minux_prefix_op(right: Object, expr_base: ExprBase) -> Result<Object, EvalError> {
    match right {
        Object::Integer(object::Integer { value, .. }) => Ok(object::Integer::new(-value).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bang_prefix_op(right: Object, expr_base: ExprBase) -> Result<Object, EvalError> {
    match right {
        Object::Integer(object::Integer { value, .. }) => Ok(object::Integer::new(!value).into()),
        Object::Boolean(object::Boolean { value, .. }) => Ok(object::Boolean::new(!value).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
