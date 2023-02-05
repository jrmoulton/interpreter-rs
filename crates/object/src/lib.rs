use std::fmt::Display;

use parser::expr::{Ident, Scope};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Object<T> {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Array(Vec<Object<T>>),
    EvalFunc { parameters: Vec<Ident>, body: Scope, env: T },
    CompFunc(Vec<bytecode::OpCode>),
    Empty,
}
impl<T> Object<T> {
    pub fn expect_bool(self) -> Option<bool> {
        if let Self::Boolean(val) = self {
            Some(val)
        } else {
            None
        }
    }
}
impl<T: std::fmt::Debug> Display for Object<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{val}"),
            Object::Boolean(val) => write!(f, "{val}"),
            Object::String(val) => write!(f, "{val}"),
            Object::Array(val) => f.debug_set().entries(val.iter()).finish(),
            Object::EvalFunc { .. } | Object::CompFunc(_) => write!(f, "Func at addr:{self:p}"),
            Object::Empty => write!(f, ""),
        }
    }
}
impl<T> Default for Object<T> {
    fn default() -> Self {
        Self::Empty
    }
}

impl<T> From<i64> for Object<T> {
    fn from(val: i64) -> Self {
        Self::Integer(val)
    }
}
impl<T> From<bool> for Object<T> {
    fn from(val: bool) -> Self {
        Self::Boolean(val)
    }
}
impl<T> From<std::string::String> for Object<T> {
    fn from(val: std::string::String) -> Self {
        Self::String(val)
    }
}
impl<T> From<()> for Object<T> {
    fn from(_: ()) -> Self {
        Self::Empty
    }
}
impl<T> From<Vec<Object<T>>> for Object<T> {
    fn from(val: Vec<Object<T>>) -> Self {
        Object::Array(val)
    }
}
