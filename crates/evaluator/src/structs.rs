use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

use error_stack::Context;
use parser::expr::Expr;

use crate::EvalObj;

#[derive(Debug)]
pub enum EvalError {
    NothingGiven,
    UnsupportedOperation(Expr),
    IdentifierNotFound(String),
    InvalidIfCondition(Expr),
    MismatchedNumOfFunctionParams,
    UnexpectedObject(EvalObj),
    IndexOutOfBounds((EvalObj, i64)),
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format = match self {
            Self::NothingGiven => "Nothing Given".into(),
            Self::IdentifierNotFound(ident_string) => {
                format!("Identifier `{ident_string}` not found")
            },
            Self::UnsupportedOperation(expr) => format!("Unsupported Operation: {expr}"),
            Self::InvalidIfCondition(expr) => format!("Invalid if condition: {expr}"),
            Self::MismatchedNumOfFunctionParams => {
                "Mismatched number of function parameters".into()
            },
            Self::UnexpectedObject(found) => format!("Unexpected object {found}"),
            Self::IndexOutOfBounds((arr_obj, index)) => {
                format!("Index out of bounds at index:{index} on object: {arr_obj}")
            },
        };
        f.write_str(&format)
    }
}
impl Context for EvalError {}

#[derive(Debug, Default)]
pub struct EnvWrapper(pub HashMap<String, EvalObj>);
impl EnvWrapper {
    pub fn new_from_map(map: HashMap<String, EvalObj>) -> Self {
        Self(map)
    }
}
impl Deref for EnvWrapper {
    type Target = HashMap<String, EvalObj>;

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
impl PartialEq for Environment {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
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
    pub fn new_from_map(map: HashMap<String, EvalObj>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: None,
        }
    }

    pub fn new_from_map_and_outer(map: HashMap<String, EvalObj>, outer: Arc<Environment>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: Some(outer),
        }
    }

    pub fn find(&self, key: &String) -> Option<EvalObj> {
        if !self.env.lock().unwrap().contains_key(key) {
            match &self.outer {
                Some(outer) => outer.find(key),
                None => None,
            }
        } else {
            self.env.lock().unwrap().get(key).cloned()
        }
    }

    pub fn set(&self, key: String, value: EvalObj) {
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
