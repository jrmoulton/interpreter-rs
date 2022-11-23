pub mod structs;
mod tests;

use std::{collections::HashMap, sync::Arc};

use error_stack::{Report, Result};
use lexer::TokenKind;
use object::EmptyWrapper;
use parser::structs::*;

use crate::{
    object::{Array, FuncIntern, Integer, Object, ObjectTrait},
    structs::*,
};

trait ExtendAssign {
    fn extend(&mut self, e: Report<EvalError>);
}
impl ExtendAssign for Option<Report<EvalError>> {
    fn extend(&mut self, e: Report<EvalError>) {
        if let Some(error) = self.as_mut() {
            error.extend_one(e);
        } else {
            *self = Some(e);
        }
    }
}

pub fn eval(statements: Vec<Statement>, env: Arc<Environment>) -> Result<Object, EvalError> {
    Report::install_debug_hook::<Suggestion>(|suggestion, context| {
        context.push_body(suggestion.to_string())
    });
    Report::install_debug_hook::<Help>(|help, context| context.push_body(help.to_string()));
    Report::install_debug_hook::<Expr>(|value, context| context.push_body(format!("{value}")));
    if cfg!(any(not(debug_assertions), test)) {
        use std::panic::Location;
        Report::install_debug_hook::<Location>(|_value, _context| {});
    }
    let mut last_obj = None;
    for statement in statements {
        match statement {
            Statement::Let { ident, expr, .. } | Statement::Assign { ident, expr, .. } => {
                let obj = eval_expr_base(expr, env.clone())?;
                if obj.is_return() {
                    return Ok(obj);
                }
                env.set(ident, obj);
            },
            Statement::Return { expr, .. } => {
                if let Some(expr) = expr {
                    let mut temp = eval_expr_base(expr, env)?;
                    temp.set_return();
                    last_obj = Some(temp);
                }
                break;
            },
            Statement::Expression { expr, terminated, .. } => {
                let obj = eval_expr_base(expr, env.clone())?;
                if obj.is_return() {
                    return Ok(obj);
                }
                if !terminated {
                    last_obj = Some(obj);
                }
            },
        };
    }
    if let Some(obj) = last_obj {
        Ok(obj)
    } else {
        Ok(EmptyWrapper.into())
    }
}

fn eval_expr_base(expr_base: Expr, env: Arc<Environment>) -> Result<Object, EvalError> {
    match expr_base {
        Expr::IntLiteral { val, .. } => Ok(val.into()),
        Expr::BoolLiteral { val, .. } => Ok(val.into()),
        Expr::StringLiteral { val, .. } => Ok(val.into()),
        Expr::FuncDef { parameters, body, .. } => Ok(Object::Function(object::Function::new(
            FuncIntern::new(parameters, body, env),
        ))),
        Expr::FuncCall { function, args, .. } => {
            let function_obj = eval_expr_base(*function, env.clone())?;
            let args: Result<Vec<Object>, EvalError> = args
                .iter()
                .map(|arg| eval_expr_base(arg.clone(), env.clone()))
                .collect();
            Ok(apply_function(function_obj, args?)?)
        },
        Expr::MethodCall { instance, method, .. } => {
            let _value = eval_expr_base(*instance, env.clone())?;
            let _value_type = _value.type_string();
            let _method_call = eval_expr_base(*method, env)?;
            // let env.find(&value_type).expect("Exists");
            unimplemented!()
        },
        Expr::Array { exprs, .. } => {
            let items: Result<Vec<Object>, EvalError> = exprs
                .iter()
                .map(|item| eval_expr_base(item.clone(), env.clone()))
                .collect();
            Ok(Object::Array(object::Array::new(items?.into())))
        },
        Expr::Index { array, index, .. } => {
            let array_obj = eval_expr_base(*array, env.clone())?;
            let index = eval_expr_base(*index, env)?;
            let index = match index {
                Object::Integer(Integer { value, .. }) => value,
                _ => Err(Report::new(EvalError::UnexpectedObject(index)))?,
            };
            match array_obj {
                Object::Array(Array { ref value, .. }) => match value.get(index as usize) {
                    Some(val) => Ok(val.clone()),
                    None => Err(Report::new(EvalError::IndexOutOfBounds((array_obj, index))))?,
                },
                Object::String(object::String { ref value, .. }) => {
                    match value.as_bytes().get(index as usize) {
                        Some(val) => Ok(Object::String(object::String::new(String::from(
                            *val as char,
                        )))),
                        None => Err(Report::new(EvalError::IndexOutOfBounds((array_obj, index))))?,
                    }
                },
                _ => Err(Report::new(EvalError::UnexpectedObject(array_obj)))?,
            }
        },
        Expr::Identifier { ref ident, .. } => match env.find(ident) {
            Some(obj) => Ok(obj),
            None => Err(
                Report::new(EvalError::IdentifierNotFound(ident.clone())).attach(Help(
                    "Variables need to be instantiated with the `let` keyword",
                )),
            )?,
        },
        Expr::Scope { statements, .. } => {
            let expr = eval(statements, env)?;
            if expr.is_return() {
                return Ok(expr);
            };
            Ok(expr)
        },
        Expr::Prefix { ref operator, ref expression, .. } => {
            let right = eval_expr_base((**expression).clone(), env)?;
            if right.is_return() {
                return Ok(right);
            }
            eval_prefix_expr(&operator.kind, right, expr_base.clone())
        },
        Expr::Binary { ref lhs, ref operator, ref rhs, .. } => {
            let left = eval_expr_base((**lhs).clone(), env.clone())?;
            if left.is_return() {
                return Ok(left);
            }
            let right = eval_expr_base((**rhs).clone(), env)?;
            if right.is_return() {
                return Ok(right);
            }
            Ok(eval_binary_expr(
                &operator.kind,
                left,
                right,
                expr_base.clone(),
            )?)
        },
        Expr::If {
            condition, consequence, alternative, ..
        } => eval_if_expr(*condition, consequence, alternative, env),
    }
}

fn apply_function(function_obj: Object, args: Vec<Object>) -> Result<Object, EvalError> {
    match function_obj {
        Object::Function(function_obj) => {
            let fn_literal = function_obj.value;
            let param_strings: Vec<String> = fn_literal
                .parameters
                .iter()
                .map(|param_ident| match &param_ident.0.kind {
                    TokenKind::Ident(ident_string) => ident_string.clone(),
                    _ => unreachable!(),
                })
                .collect();
            if param_strings.len() != args.len() {
                Err(Report::new(EvalError::MismatchedNumOfFunctionParams))?
            }
            let mut new_env = HashMap::new();
            param_strings.iter().zip(args).for_each(|(param, object)| {
                new_env.insert(param.clone(), object);
            });
            eval(
                fn_literal.body.statements,
                Arc::new(Environment::new_from_map_and_outer(new_env, fn_literal.env)),
            )
        },
        _ => unreachable!("No other objects match to calling this function {function_obj}"),
    }
}

fn eval_if_expr(
    condition: Expr, consequence: Scope, alternative: Option<ElseIfExpr>, env: Arc<Environment>,
) -> Result<Object, EvalError> {
    let cond_bool = eval_expr_base(condition.clone(), env.clone())?;
    match cond_bool {
        Object::Boolean(object::Boolean { value, .. }) => {
            if value {
                eval(consequence.statements, env)
            } else if let Some(alt) = alternative {
                match alt {
                    ElseIfExpr::ElseIf(else_expr_base) => eval_if_expr(
                        *else_expr_base.condition,
                        else_expr_base.consequence,
                        else_expr_base.alternative,
                        env,
                    ),
                    ElseIfExpr::Else(if_expr) => eval(if_expr.statements, env),
                }
            } else {
                Ok(object::Empty::new(().into()).into())
            }
        },
        _ => Err(Report::new(EvalError::InvalidIfCondition(condition)))?,
    }
}

#[allow(unreachable_patterns)]
fn eval_binary_expr(
    operator: &TokenKind, left: Object, right: Object, expr_base: Expr,
) -> Result<Object, EvalError> {
    match left {
        Object::Integer(_) => eval_int_binary_expr(operator, left, right, expr_base),
        Object::Boolean(_) => eval_bool_binary_expr(operator, left, right, expr_base),
        Object::String(_) => eval_string_binary_expr(operator, left, right, expr_base),
        _ => unimplemented!(),
    }
}

fn eval_string_binary_expr(
    operator: &TokenKind, left: Object, right: Object, expr_base: Expr,
) -> Result<Object, EvalError> {
    let left = match left {
        Object::String(object::String { value, .. }) => value,
        _ => unreachable!("string was already matched"),
    };
    let right = match right {
        Object::String(object::String { value, .. }) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        TokenKind::Eq => Ok(object::Boolean::new(left == right).into()),
        TokenKind::Ne => Ok(object::Boolean::new(left != right).into()),
        TokenKind::Plus => Ok(object::String::new(left + &right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bool_binary_expr(
    operator: &TokenKind, left: Object, right: Object, expr_base: Expr,
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
        TokenKind::Eq => Ok(object::Boolean::new(left == right).into()),
        TokenKind::Ne => Ok(object::Boolean::new(left != right).into()),
        TokenKind::Or => Ok(object::Boolean::new(left || right).into()),
        TokenKind::And => Ok(object::Boolean::new(left && right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_int_binary_expr(
    operator: &TokenKind, left: Object, right: Object, expr_base: Expr,
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
        TokenKind::Plus => Ok(object::Integer::new(left + right).into()),
        TokenKind::Minus => Ok(object::Integer::new(left - right).into()),
        TokenKind::Slash => Ok(object::Integer::new(left / right).into()),
        TokenKind::Asterisk => Ok(object::Integer::new(left * right).into()),
        TokenKind::LT => Ok(object::Boolean::new(left < right).into()),
        TokenKind::GT => Ok(object::Boolean::new(left > right).into()),
        TokenKind::Eq => Ok(object::Boolean::new(left == right).into()),
        TokenKind::Ne => Ok(object::Boolean::new(left != right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_prefix_expr(
    operator: &TokenKind, right: Object, expr_base: Expr,
) -> Result<Object, EvalError> {
    match operator {
        TokenKind::Minus => eval_minux_prefix_op(right, expr_base),
        TokenKind::Bang => eval_bang_prefix_op(right, expr_base),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_minux_prefix_op(right: Object, expr_base: Expr) -> Result<Object, EvalError> {
    match right {
        Object::Integer(object::Integer { value, .. }) => Ok(object::Integer::new(-value).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bang_prefix_op(right: Object, expr_base: Expr) -> Result<Object, EvalError> {
    match right {
        Object::Integer(object::Integer { value, .. }) => Ok(object::Integer::new(!value).into()),
        Object::Boolean(object::Boolean { value, .. }) => Ok(object::Boolean::new(!value).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
