use std::any::Any;
use std::fmt::Display;
use std::sync::Arc;

use parser::expr::{Ident, Scope};

use crate::structs::Environment;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Array(Vec<Object>),
    EvalFunc { parameters: Vec<Ident>, body: Scope, env: Arc<Environment> },
    Empty,
}
impl Object {
    pub fn expect_bool(self) -> Option<bool> {
        if let Self::Boolean(val) = self {
            Some(val)
        } else {
            None
        }
    }
}
impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{val}"),
            Object::Boolean(val) => write!(f, "{val}"),
            Object::String(val) => write!(f, "{val}"),
            Object::Array(val) => f.debug_set().entries(val.iter()).finish(),
            Object::EvalFunc { .. } => write!(f, "Func at addr:{self:p}"),
            Object::Empty => write!(f, ""),
        }
    }
}
impl Default for Object {
    fn default() -> Self {
        Self::Empty
    }
}

impl From<i64> for Object {
    fn from(val: i64) -> Self {
        Self::Integer(val)
    }
}
impl From<bool> for Object {
    fn from(val: bool) -> Self {
        Self::Boolean(val)
    }
}
impl From<std::string::String> for Object {
    fn from(val: std::string::String) -> Self {
        Self::String(val)
    }
}
impl From<()> for Object {
    fn from(_: ()) -> Self {
        Self::Empty
    }
}
impl From<Vec<Object>> for Object {
    fn from(val: Vec<Object>) -> Self {
        Object::Array(val)
    }
}

#[derive(Clone, Debug)]
pub struct EvalObj {
    pub(crate) is_return: bool,
    pub(crate) obj: Object,
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
            Object::Empty => &(),
        }
    }
}
impl Default for EvalObj {
    fn default() -> Self {
        Self { is_return: false, obj: ().into() }
    }
}
impl From<Object> for EvalObj {
    fn from(val: Object) -> Self {
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
