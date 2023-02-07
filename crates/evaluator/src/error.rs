use std::fmt::Display;

use error_stack::Context;
use parser::expr::Expr;

use crate::EvalObj;

#[derive(Debug)]
pub enum EvalError {
    NothingGiven,
    UnsupportedOperation(Expr),
    IdentifierNotFound(String),
    InvalidIfCondition(Expr),
    MismatchedNumOfFunctionParams,
    UnexpectedObject(EvalObj),
    IndexOutOfBounds((EvalObj, i64)),
}
impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format = match self {
            Self::NothingGiven => "Nothing Given".into(),
            Self::IdentifierNotFound(ident_string) => {
                format!("Identifier `{ident_string}` not found")
            },
            Self::UnsupportedOperation(expr) => format!("Unsupported Operation: {expr}"),
            Self::InvalidIfCondition(expr) => format!("Invalid if condition: {expr}"),
            Self::MismatchedNumOfFunctionParams => {
                "Mismatched number of function parameters".into()
            },
            Self::UnexpectedObject(found) => format!("Unexpected object {found}"),
            Self::IndexOutOfBounds((arr_obj, index)) => {
                format!("Index out of bounds at index:{index} on object: {arr_obj}")
            },
        };
        f.write_str(&format)
    }
}
impl Context for EvalError {}
