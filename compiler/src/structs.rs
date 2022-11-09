use std::{collections::HashMap, fmt::Debug};

use bytecode::OpCode;
use evaluator::object::{EmptyWrapper, Object};

pub struct Compiler {
    pub constants: HashMap<evaluator::object::Object, usize>,
    pub bytecode: Vec<OpCode>,
}
impl Debug for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Compiler")
            .field("bytecode", &self.bytecode)
            .finish()
    }
}
pub enum OpType {
    Binary,
    Prefix,
    Other,
}

impl Compiler {
    pub fn new() -> Self {
        let mut constants = HashMap::new();
        constants.insert(EmptyWrapper::new().into(), 0);
        Self {
            constants,
            bytecode: Vec::new(),
        }
    }

    pub fn get_fields(self) -> (Vec<OpCode>, Vec<evaluator::object::Object>) {
        let mut x = self.constants.into_iter().collect::<Vec<(Object, usize)>>();
        x.sort_by_key(|k| k.1);
        let mut new_vec = Vec::new();
        for val in x {
            new_vec.push(val.0);
        }
        (self.bytecode, new_vec)
    }
}
impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
