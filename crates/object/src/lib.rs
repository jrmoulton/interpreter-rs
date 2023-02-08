mod env;

use std::{any::Any, fmt::Display, sync::Arc};

#[cfg(feature = "evaluate")]
pub use env::Environment;
#[cfg(feature = "evaluate")]
use parser::expr::{Ident, Scope};

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Array(Vec<Object>),
    #[cfg(feature = "evaluate")]
    EvalFunc {
        parameters: Vec<Ident>,
        body: Scope,
        env: Arc<Environment>,
    },
    #[cfg(feature = "compile")]
    CompFunc(Vec<bytecode::OpCode>),
    Empty,
}
impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Array(a), Object::Array(b)) => a == b,
            (Object::Empty, Object::Empty) => true,
            #[cfg(feature = "evaluate")]
            (
                Object::EvalFunc {
                    parameters: l_parameters,
                    body: l_body,
                    ..
                },
                Object::EvalFunc {
                    parameters: r_parameters,
                    body: r_body,
                    ..
                },
            ) => l_parameters == r_parameters && l_body == r_body,
            #[cfg(feature = "compile")]
            (Object::CompFunc(l_bc), Object::CompFunc(r_bc)) => l_bc == r_bc,
            _ => false,
        }
    }
}
impl Eq for Object {}
impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            Object::Array(a) => a.hash(state),
            #[cfg(feature = "evaluate")]
            Object::EvalFunc { parameters, body, .. } => {
                parameters.hash(state);
                body.hash(state);
            },
            #[cfg(feature = "compile")]
            Object::CompFunc(b) => b.hash(state),
            Object::Empty => (),
        }
    }
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
            #[cfg(feature = "evaluate")]
            Object::EvalFunc { .. } => write!(f, "Func at addr:{self:p}"),
            #[cfg(feature = "compile")]
            Object::CompFunc(_) => write!(f, "Func at addr:{self:p}"),
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
    pub obj: Object,
}
impl std::fmt::Display for EvalObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.obj))
    }
}
impl EvalObj {
    pub fn is_return(&self) -> bool {
        self.is_return
    }

    pub fn set_return(&mut self) {
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
