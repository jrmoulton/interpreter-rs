use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Array(Vec<Object>),
    CompFunc(Vec<bytecode::OpCode>),
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
