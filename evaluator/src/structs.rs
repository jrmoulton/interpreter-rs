use std::{
    collections::HashMap,
    fmt::{Display, Write as _},
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

use error_stack::{Context, Report};
use parser::structs::*;

use crate::object::Object;

#[derive(Debug)]
pub enum EvalError {
    UnsupportedOperation(ExprBase),
    IdentifierNotFound(String),
    InvalidIfCondition(ExprBase),
    MismatchedNumOfFunctionParams,
    UnexpectedObject(Object),
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format = match self {
            Self::IdentifierNotFound(ident_string) => {
                format!("Identifier `{ident_string}` not found")
            }
            Self::UnsupportedOperation(expr) => format!("Unsupported Operation: {expr}"),
            Self::InvalidIfCondition(expr) => format!("Invalid if condition: {expr}"),
            Self::MismatchedNumOfFunctionParams => {
                "Mismatched number of function parameters".into()
            }
            Self::UnexpectedObject(found) => format!("Unexpected object {found}"),
        };
        f.write_str(&format)
    }
}
impl Context for EvalError {}

#[derive(Debug)]
pub struct EvalErrors {
    pub errors: Vec<Report<EvalError>>,
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
        self.errors.iter().for_each(|val| {
            write!(ret_str, "{}", val);
        });
        ret_str.push_str(" ]");
        f.write_str(&ret_str)
    }
}

#[derive(Debug, Default)]
pub struct EnvWrapper(pub HashMap<String, Object>);
impl EnvWrapper {
    pub fn new_from_map(map: HashMap<String, Object>) -> Self {
        Self(map)
    }
}
impl Deref for EnvWrapper {
    type Target = HashMap<String, Object>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for EnvWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct Environment {
    pub env: Mutex<EnvWrapper>,
    pub outer: Option<Arc<Environment>>,
}
impl Default for Environment {
    fn default() -> Self {
        Self {
            env: Mutex::new(EnvWrapper::default()),
            outer: None,
        }
    }
}
impl Environment {
    pub fn new_from_map(map: HashMap<String, Object>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: None,
        }
    }
    pub fn new_from_map_and_outer(map: HashMap<String, Object>, outer: Arc<Environment>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: Some(outer),
        }
    }
    pub fn find(&self, key: &String) -> Option<Object> {
        if !self.env.lock().unwrap().contains_key(key) {
            match &self.outer {
                Some(outer) => outer.find(key),
                None => None,
            }
        } else {
            self.env.lock().unwrap().get(key).cloned()
        }
    }
    pub fn set(&self, key: String, value: Object) {
        self.env.lock().unwrap().insert(key, value);
    }
    pub fn has(&self, key: &String) -> bool {
        if !self.env.lock().unwrap().contains_key(key) {
            match &self.outer {
                Some(outer) => outer.has(key),
                None => false,
            }
        } else {
            true
        }
    }
}
