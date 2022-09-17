use derive_object::Object;
use std::{
    any::Any,
    fmt::{Debug, Display, Formatter},
};

use enum_dispatch::enum_dispatch;

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
}

#[enum_dispatch]
#[derive(Debug)]
pub(crate) enum Object {
    Integer,
    Boolean,
    Empty,
}
impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Object::Integer(inner) => format!("{}", inner),
                Object::Boolean(inner) => format!("{}", inner),
                Object::Empty(inner) => format!("{}", inner),
            }
        ))
    }
}

#[derive(Debug, Object)]
pub(crate) struct Integer {
    pub value: i64,
}

#[derive(Debug, Object)]
pub(crate) struct Boolean {
    pub value: bool,
}

#[derive(Debug, Object)]
pub(crate) struct Empty {
    pub value: (),
}
