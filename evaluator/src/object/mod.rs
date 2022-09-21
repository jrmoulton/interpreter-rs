use std::{
    any::Any,
    fmt::{Debug, Display},
    rc::Rc,
};

use enum_dispatch::enum_dispatch;
use parser::structs::{Ident, Scope};

use crate::Environment;

#[macro_use]
mod literal_types_macro;

#[derive(Debug, Clone)]
pub struct FuncIntern {
    pub parameters: Vec<Ident>,
    pub body: Scope,
    pub env: Rc<Environment>,
}
impl FuncIntern {
    pub fn new(parameters: Vec<Ident>, body: Scope, env: Rc<Environment>) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}

make_literal_types!(
    (Integer, i64),
    (Boolean, bool),
    (Empty, ()),
    (Function, FuncIntern),
    (String, std::string::String)
);

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
    fn set_return(&mut self);
    fn is_return(&self) -> bool;
}
