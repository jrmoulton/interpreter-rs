use error_stack::{Report, Result};
use object::EvalObj;

use crate::error::EvalError;

trait ExtendAssign {
    fn extend(&mut self, e: Report<EvalError>);
}
impl ExtendAssign for Option<Report<EvalError>> {
    fn extend(&mut self, e: Report<EvalError>) {
        if let Some(error) = self.as_mut() {
            error.extend_one(e);
        } else {
            *self = Some(e);
        }
    }
}

pub(crate) type EvalResult = Result<EvalObj, EvalError>;
