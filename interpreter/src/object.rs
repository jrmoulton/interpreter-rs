use std::{
    any::Any,
    fmt::{Debug, Display},
};

use enum_dispatch::enum_dispatch;

use crate::{
    evaluator::Environment,
    parser::structs::{Ident, Scope},
};

#[macro_use]
mod literal_types_macro;

#[derive(Debug, Clone)]
pub(crate) struct FuncIntern {
    pub parameters: Vec<Ident>,
    pub body: Scope,
    pub env: Environment,
}
impl FuncIntern {
    pub fn new(parameters: Vec<Ident>, body: Scope, env: Environment) -> Self {
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
    (Function, FuncIntern)
);

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
    fn set_return(&mut self);
    fn is_return(&self) -> bool;
}
