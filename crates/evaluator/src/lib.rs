pub mod structs;
mod tests;

use std::{any::Any, collections::HashMap, sync::Arc};

use error_stack::{Report, Result};
use lexer::TokenKind;
use object::Object;
use parser::structs::*;

use crate::structs::*;

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

#[derive(Clone, Debug)]
pub struct EvalObj {
    is_return: bool,
    obj: object::Object<Arc<Environment>>,
}
impl std::fmt::Display for EvalObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.obj))
    }
}
impl EvalObj {
    fn is_return(&self) -> bool {
        self.is_return
    }

    fn set_return(&mut self) {
        self.is_return = true;
    }

    pub fn inner(&self) -> &dyn Any {
        match &self.obj {
            Object::Integer(val) => val,
            Object::Boolean(val) => val,
            Object::String(val) => val,
            Object::Array(val) => val,
            Object::EvalFunc { .. } => &None::<()>,
            Object::CompFunc(_) => &None::<()>,
            Object::Empty => &(),
        }
    }
}
impl Default for EvalObj {
    fn default() -> Self {
        Self { is_return: false, obj: ().into() }
    }
}
impl From<Object<Arc<Environment>>> for EvalObj {
    fn from(val: Object<Arc<Environment>>) -> Self {
        Self { is_return: false, obj: val }
    }
}

impl From<i64> for EvalObj {
    fn from(val: i64) -> Self {
        Self {
            obj: val.into(),
            ..Default::default()
        }
    }
}
impl From<bool> for EvalObj {
    fn from(val: bool) -> Self {
        Self {
            obj: val.into(),
            ..Default::default()
        }
    }
}
impl From<std::string::String> for EvalObj {
    fn from(val: std::string::String) -> Self {
        Self {
            obj: val.into(),
            ..Default::default()
        }
    }
}
impl From<()> for EvalObj {
    fn from(_: ()) -> Self {
        Self { ..Default::default() }
    }
}
impl From<Vec<EvalObj>> for EvalObj {
    fn from(mut val: Vec<EvalObj>) -> Self {
        Self {
            obj: Object::Array(
                val.iter_mut()
                    .map(|val| std::mem::take(&mut val.obj))
                    .collect(),
            ),
            ..Default::default()
        }
    }
}

type EvalResult = Result<EvalObj, EvalError>;

pub fn eval(statements: Vec<Statement>, env: Arc<Environment>) -> EvalResult {
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
        Ok(Object::Empty.into())
    }
}

fn eval_expr_base(expr_base: Expr, env: Arc<Environment>) -> EvalResult {
    match expr_base {
        Expr::IntLiteral { val, .. } => Ok(val.into()),
        Expr::BoolLiteral { val, .. } => Ok(val.into()),
        Expr::StringLiteral { val, .. } => Ok(val.into()),
        Expr::FuncDef { parameters, body, .. } => {
            Ok(Object::EvalFunc { parameters, body, env }.into())
        },
        Expr::FuncCall { function, args, .. } => {
            let function_obj = eval_expr_base(*function, env.clone())?;
            let args: Result<Vec<EvalObj>, EvalError> = args
                .iter()
                .map(|arg| eval_expr_base(arg.clone(), env.clone()))
                .collect();
            Ok(apply_function(function_obj, args?)?)
        },
        Expr::MethodCall { instance, method, .. } => {
            let _value = eval_expr_base(*instance, env.clone())?;
            // let _value_type = _value.type_string();
            let _method_call = eval_expr_base(*method, env)?;
            // let env.find(&value_type).expect("Exists");
            unimplemented!()
        },
        Expr::Array { exprs, .. } => {
            let items: Result<Vec<EvalObj>, EvalError> = exprs
                .iter()
                .map(|item| eval_expr_base(item.clone(), env.clone()))
                .collect();
            Ok(items?.into())
        },
        Expr::Index { array, index, .. } => {
            let array_obj = eval_expr_base(*array, env.clone())?;
            let index = eval_expr_base(*index, env)?;
            let index = match index.obj {
                Object::Integer(val) => val,
                _ => Err(Report::new(EvalError::UnexpectedObject(index)))?,
            };
            match array_obj.obj {
                Object::Array(ref val) => match val.get(index as usize) {
                    Some(val) => Ok(val.clone().into()),
                    None => Err(Report::new(EvalError::IndexOutOfBounds((array_obj, index))))?,
                },
                Object::String(ref val) => match val.as_bytes().get(index as usize) {
                    Some(val) => Ok((*val as char).to_string().into()),
                    None => Err(Report::new(EvalError::IndexOutOfBounds((array_obj, index))))?,
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

fn apply_function(function_obj: EvalObj, args: Vec<EvalObj>) -> EvalResult {
    match function_obj.obj {
        Object::EvalFunc { mut parameters, body, env } => {
            let param_strings: Vec<String> = parameters
                .iter_mut()
                .map(|param_ident| std::mem::take(&mut param_ident.ident))
                .collect();
            if param_strings.len() != args.len() {
                Err(Report::new(EvalError::MismatchedNumOfFunctionParams))?
            }
            let mut new_env = HashMap::new();
            param_strings.iter().zip(args).for_each(|(param, object)| {
                new_env.insert(param.clone(), object);
            });
            eval(
                body.statements,
                Arc::new(Environment::new_from_map_and_outer(new_env, env)),
            )
        },
        _ => unreachable!("No other objects match to calling this function {function_obj}"),
    }
}

fn eval_if_expr(
    condition: Expr, consequence: Scope, alternative: Option<ElseIfExpr>, env: Arc<Environment>,
) -> EvalResult {
    let cond_bool = eval_expr_base(condition.clone(), env.clone())?;
    match cond_bool.obj {
        Object::Boolean(val) => {
            if val {
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
                Ok(Object::Empty.into())
            }
        },
        _ => Err(Report::new(EvalError::InvalidIfCondition(condition)))?,
    }
}

#[allow(unreachable_patterns)]
fn eval_binary_expr(
    operator: &TokenKind, left: EvalObj, right: EvalObj, expr_base: Expr,
) -> EvalResult {
    match left.obj {
        Object::Integer(_) => eval_int_binary_expr(operator, left, right, expr_base),
        Object::Boolean(_) => eval_bool_binary_expr(operator, left, right, expr_base),
        Object::String(_) => eval_string_binary_expr(operator, left, right, expr_base),
        _ => unimplemented!(),
    }
}

fn eval_string_binary_expr(
    operator: &TokenKind, left: EvalObj, right: EvalObj, expr_base: Expr,
) -> EvalResult {
    let left = match left.obj {
        Object::String(val) => val,
        _ => unreachable!("string was already matched"),
    };
    let right = match right.obj {
        Object::String(val) => val,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        TokenKind::Eq => Ok((left == right).into()),
        TokenKind::Ne => Ok((left != right).into()),
        TokenKind::Plus => Ok((left + &right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bool_binary_expr(
    operator: &TokenKind, left: EvalObj, right: EvalObj, expr_base: Expr,
) -> EvalResult {
    let left = match left.obj {
        Object::Boolean(val) => val,
        _ => unreachable!("boolean was already matched"),
    };
    let right = match right.obj {
        Object::Boolean(value) => value,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        TokenKind::Eq => Ok((left == right).into()),
        TokenKind::Ne => Ok((left != right).into()),
        TokenKind::Or => Ok((left || right).into()),
        TokenKind::And => Ok((left && right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_int_binary_expr(
    operator: &TokenKind, left: EvalObj, right: EvalObj, expr_base: Expr,
) -> EvalResult {
    let left = match left.obj {
        Object::Integer(val) => val,
        _ => unreachable!(),
    };
    let right = match right.obj {
        Object::Integer(val) => val,
        _ => unimplemented!("Here I would add support for operator overloading"),
    };
    match operator {
        TokenKind::Plus => Ok((left + right).into()),
        TokenKind::Minus => Ok((left - right).into()),
        TokenKind::Slash => Ok((left / right).into()),
        TokenKind::Asterisk => Ok((left * right).into()),
        TokenKind::LT => Ok((left < right).into()),
        TokenKind::GT => Ok((left > right).into()),
        TokenKind::Eq => Ok((left == right).into()),
        TokenKind::Ne => Ok((left != right).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_prefix_expr(operator: &TokenKind, right: EvalObj, expr_base: Expr) -> EvalResult {
    match operator {
        TokenKind::Minus => eval_minux_prefix_op(right, expr_base),
        TokenKind::Bang => eval_bang_prefix_op(right, expr_base),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_minux_prefix_op(right: EvalObj, expr_base: Expr) -> EvalResult {
    match right.obj {
        Object::Integer(val) => Ok((-val).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}

fn eval_bang_prefix_op(right: EvalObj, expr_base: Expr) -> EvalResult {
    match right.obj {
        Object::Integer(val) => Ok((!val).into()),
        Object::Boolean(val) => Ok((!val).into()),
        _ => Err(Report::new(EvalError::UnsupportedOperation(expr_base))),
    }
}
