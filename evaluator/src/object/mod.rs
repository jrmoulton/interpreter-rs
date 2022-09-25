use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
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

#[derive(Debug, Clone)]
pub struct ClassObject {
    pub fields: HashMap<String, Object>,
    pub methods: HashMap<String, Object>,
}

make_literal_types!(
    (Integer, i64, "Integer"),
    (Boolean, bool, "Boolean"),
    (Empty, (), "Empty"),
    (Function, FuncIntern, "Function"),
    (String, std::string::String, "String"),
    (Array, std::vec::Vec<Object>, "Array")
    (Class, ClassObject, "Class")
);

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
    fn set_return(&mut self);
    fn is_return(&self) -> bool;
    fn type_string(&self) -> std::string::String;
}
