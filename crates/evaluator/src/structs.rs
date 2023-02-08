use std::{
    any::Any,
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

use error_stack::{Report, Result};
use object::Object;

use crate::error::EvalError;

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
    pub(crate) is_return: bool,
    pub(crate) obj: object::Object<Arc<Environment>>,
}
impl std::fmt::Display for EvalObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.obj))
    }
}
impl EvalObj {
    pub(crate) fn is_return(&self) -> bool {
        self.is_return
    }

    pub(crate) fn set_return(&mut self) {
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

pub(crate) type EvalResult = Result<EvalObj, EvalError>;
