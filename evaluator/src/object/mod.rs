use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    ops::{Deref, DerefMut},
    sync::Arc,
};

use enum_dispatch::enum_dispatch;
use parser::structs::{Ident, Scope};

use crate::Environment;

#[macro_use]
mod literal_types_macro;

#[derive(Clone)]
pub struct FuncIntern {
    pub parameters: Vec<Ident>,
    pub body: Scope,
    pub env: Arc<Environment>,
}
impl Debug for FuncIntern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}{:?}", self.parameters, self.body))
    }
}
impl FuncIntern {
    pub fn new(parameters: Vec<Ident>, body: Scope, env: Arc<Environment>) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}
impl Display for FuncIntern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret_str = std::string::String::from("(");
        for param in self.parameters.iter() {
            write!(ret_str, "{param}, ")?;
        }
        ret_str.push(')');
        f.write_fmt(format_args!("function {ret_str}{{...}}"))
    }
}

#[derive(Debug, Clone)]
pub struct ArrayWrapper(pub Vec<Object>);
impl Display for ArrayWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret_str = std::string::String::from("[ ");
        for val in &self.0 {
            write!(ret_str, "{}", val)?;
        }
        ret_str.push_str(" ]");
        f.write_str(&ret_str)
    }
}
impl Deref for ArrayWrapper {
    type Target = Vec<Object>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for ArrayWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl From<Vec<Object>> for ArrayWrapper {
    fn from(arr: Vec<Object>) -> Self {
        ArrayWrapper(arr)
    }
}

#[derive(Debug, Clone)]
pub struct ClassObject {
    pub fields: HashMap<String, Object>,
    pub methods: HashMap<String, Object>,
}
impl Display for ClassObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Class: {:?}", self.fields))
    }
}

#[derive(Debug, Clone)]
pub struct EmptyWrapper(pub ());
impl Display for EmptyWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("()")
    }
}
impl From<()> for EmptyWrapper {
    fn from(_: ()) -> Self {
        Self(())
    }
}

make_literal_types!(
    (Integer, i64, "Integer"),
    (Boolean, bool, "Boolean"),
    (Empty, EmptyWrapper, "Empty"),
    (Function, FuncIntern, "Function"),
    (String, std::string::String, "String"),
    (Array, ArrayWrapper, "Array"),
    (Class, ClassObject, "Class")
);

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
    fn set_return(&mut self);
    fn is_return(&self) -> bool;
    fn type_string(&self) -> std::string::String;
}
