use std::{
    fmt::{Debug, Display, Write},
    hash::Hash,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use enum_dispatch::enum_dispatch;
use parser::structs::{Ident, Scope};

#[macro_use]
mod literal_types_macro;

#[derive(Clone)]
pub struct FuncIntern {
    pub parameters: Vec<Ident>,
    pub body: Scope,
    pub env: Arc<Environment>,
}
impl PartialEq for FuncIntern {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters && self.body == other.body
    }
}
impl Hash for FuncIntern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parameters.hash(state);
        self.body.hash(state);
    }
}
impl Eq for FuncIntern {}
impl Debug for FuncIntern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}{:?}", self.parameters, self.body))
    }
}
impl FuncIntern {
    pub fn new(parameters: Vec<Ident>, body: Scope, env: Arc<Environment>) -> Self {
        Self { parameters, body, env }
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
#[derive(Clone)]
pub struct CFunc {
    instucts: Vec<Byte
}
impl PartialEq for CFunc {
    fn eq(&self, other: &Self) -> bool {    self.parameters.hash(state);
        self.body.hash(state);
    }
impl Eq for CFunc {}
impl Debug for CFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}{:?}", self.parameters, self.body))
    }
}
impl CFunc {
    pub fn new(parameters: Vec<Ident>, body: Scope, env: Arc<Environment>) -> Self {
        Self { parameters, body, env }
    }
}
impl Display for CFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret_str = std::string::String::from("(");
        for param in self.parameters.iter() {
            write!(ret_str, "{param}, ")?;
        }
        ret_str.push(')');
        f.write_fmt(format_args!("function {ret_str}{{...}}"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayWrapper(pub Vec<Object>);
impl Display for ArrayWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret_str = std::string::String::from("[ ");
        for val in &self.0 {
            write!(ret_str, "{}, ", val)?;
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EmptyWrapper;
impl Display for EmptyWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("()")
    }
}
impl From<()> for EmptyWrapper {
    fn from(_: ()) -> Self {
        Self
    }
}
impl EmptyWrapper {
    pub fn new() -> Self {
        Self
    }
}
impl Default for EmptyWrapper {
    fn default() -> Self {
        Self::new()
    }
}

make_literal_types!(
    (Integer, expect_int, i64, "Integer"),
    (Boolean, expect_bool, bool, "Boolean"),
    (Empty, expect_empty, EmptyWrapper, "Empty"),
    (Function, expect_func, FuncIntern, "Function"),
    (String, expect_string, std::string::String, "String"),
    (Array, expect_arr, ArrayWrapper, "Array"),
    (Cfunc, expect_cfunc,)
);

#[enum_dispatch(Object)]
pub(crate) trait ObjectTrit: Display + Debug + Eq + PartialEq + Hash {
    fn inner(&self) -> &dyn std::any::Any;
    fn set_return(&mut self);
    fn is_return(&self) - bool;
    fn type_string(&self) -> std::string::String;
}
