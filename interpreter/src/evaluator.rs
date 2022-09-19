use std::{cell::RefCell, collections::HashMap, fmt::{Display, Write as _}, rc::Rc, ops::{Deref, DerefMut}};

use error_stack::{Context, Report, Result};

use crate::{
    lexer::{LocTok, Token},
    object::{self, FuncIntern, Object, ObjectTrait},
    parser::structs::*,
};

mod tests;

#[derive(Debug)]
pub(crate) enum EvalError {
    UnsupportedOperation(ExprBase),
    IdentifierNotFound(String),
    InvalidIfCondition(ExprBase),
    MismatchedNumOfFunctionParams,
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format = match self {
            Self::IdentifierNotFound(ident_string) => format!("Identifier `{ident_string}` not found"),
            Self::UnsupportedOperation(expr) => format!("Unsupported Operation: {expr}"),
            Self::InvalidIfCondition(expr) => format!("Invalid if condition: {expr}"),
            Self::MismatchedNumOfFunctionParams => "Mismatched number of function parameters".into(),

        };
        f.write_str(&format)
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
impl Display for EvalErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret_str = String::from("[ ");
        self.errors.iter().for_each(|val| {let _ = write!(ret_str, "{}", val);} );
        ret_str.push_str(" ]");
        f.write_str(&ret_str)
    }
}


// need to make find method look in outer environment

#[derive(Debug)]
pub(crate) struct EnvWrapper{ 
    pub env: HashMap<String, Object> ,
    pub outer: Option<Box<EnvWrapper>>
}
impl Deref for EnvWrapper {
    type Target = HashMap<String, Object>;
    fn deref(&self) -> &Self::Target {
        &self.env
    }
}
impl DerefMut for EnvWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.env
    }
}
impl EnvWrapper {
    pub fn new() -> Self {
        Self {
            env:HashMap::new(),
            outer: None
        }
    }
    pub fn new_from_map(map: HashMap<String, Object>) -> Self {
        Self {
            env: map,
            outer: None
        }
    }
    pub fn find(&self, key: &String) -> Option<&Object> {
        if !self.env.contains_key(key) {
            match &self.outer {
                Some(outer) => outer.find(key),
                None => None,
            }
        } else {
            self.get(key) 
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Environment(pub Rc<RefCell<EnvWrapper>>);
impl Deref for Environment {
    type Target = Rc<RefCell<EnvWrapper>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Environment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Environment {
    pub fn new() -> Self {
        Environment(Rc::new(RefCell::new(EnvWrapper::new())))
    }
    pub fn new_from_map(map: HashMap<String, Object>) -> Self {
        Environment(Rc::new(RefCell::new(EnvWrapper::new_from_map(map))))
    }
}

pub(crate) fn eval(
    statements: Vec<Statement>,
    env: Environment,
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
                env.borrow_mut().insert(ident_string, expr_obj);
                Ok((object::Empty::new(()).into(), true))
            }
            Statement::Return(expr) => {
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
                let key_exists = env.borrow_mut().contains_key(&ident_string.to_string());
                let expr_obj = eval_expr_base(*expr, env.clone())?;
                if key_exists {
                    env.borrow_mut().insert(ident_string.into(), expr_obj);
                } else {
                    Err(Report::new(EvalError::IdentifierNotFound(ident_string.clone())).attach(ident))?
                }
                Ok((object::Empty::new(()).into(), true))
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
                Ok(Object::Empty(object::Empty::new(())))
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
    env: Environment,
) -> std::result::Result<(Object, bool), EvalErrors> {
    match expr {
        Expr::Terminated(expr_base) => Ok((eval_expr_base(expr_base, env.clone())?, true)),
        Expr::NonTerminated(expr_base) => Ok((eval_expr_base(expr_base, env.clone())?, false)),
    }
}

fn eval_expr_base(
    expr_base: ExprBase,
    env: Environment,
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
        ExprBase::FuncLiteral(FnLiteral { parameters, body }) => Ok(Object::Function(object::Function::new(FuncIntern::new(parameters, body, env.clone())))),
        ExprBase::CallExpression(CallExpr { function, args }) => {
            let function_obj = eval_expr_base(*function, env.clone())?;
            let args: std::result::Result<Vec<(Object, bool)>, EvalErrors> = args.iter().map(|arg| eval_expression(arg.clone(), env.clone())).collect();
            let args: Vec<Object> = args?.iter().map(|val| val.clone().0).collect();
            Ok(apply_function(function_obj, args)?)
        },
        ExprBase::Identifier(Ident(LocTok {
            token: Token::Ident(ref ident_string),
            ..
        })) => {
                match env.borrow_mut().find(ident_string) {
                    Some(obj) => Ok(obj.clone()),
                    None => Err(Report::new(EvalError::IdentifierNotFound(ident_string.clone())).attach(expr_base))?
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

fn apply_function(function_obj: Object, args: Vec<Object>) -> std::result::Result<Object, EvalErrors> {
    let mut new_env = HashMap::new();
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
            param_strings.iter().zip(args).for_each(|(param, object)| {new_env.insert(param.clone(), object);});
            eval(fn_literal.body.statements, Environment::new_from_map(new_env))
        },
        _ => unreachable!("No other objects match to calling this function")
    }
}

fn eval_if_expr(
    condition: &ExprBase,
    consequence: Scope,
    alternative: Option<ElseIfExpr>,
    env: Environment,
) -> std::result::Result<Object, EvalErrors> {
    let cond_bool = eval_expr_base((*condition).clone(), env.clone())?;
    match cond_bool {
        Object::Boolean(object::Boolean { value, .. }) => {
            if value {
                eval(consequence.statements, env.clone())
            } else if let Some(alt) = alternative {
                match alt {
                    ElseIfExpr::ElseIf(else_expr_base) => {
                        eval_expr_base(*else_expr_base, env.clone())
                    }
                    ElseIfExpr::Else(if_expr) => eval(if_expr.statements, env.clone()),
                }
            } else {
                Ok(Object::Empty(object::Empty::new(())))
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
        _ => unimplemented!(),
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
