use std::{
    any::Any,
    fmt::{Debug, Display, Formatter},
};

use enum_dispatch::enum_dispatch;

#[macro_use]
mod literal_types_macro;

make_literal_types!((Integer, i64), (Boolean, bool), (Empty, ()));

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrait: Display + Debug {
    fn inner(&self) -> &dyn Any;
    fn set_return(&mut self);
    fn is_return(&self) -> bool;
}
