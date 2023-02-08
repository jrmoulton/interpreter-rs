use std::{collections::HashMap, fmt::Debug};

use crate::object::Object;

pub struct Compiler {
    pub constants: HashMap<Object, usize>,
    pub const_vec: Vec<Object>,
}
impl Debug for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Compiler")
            .field("constants", &self.constants)
            .finish()
    }
}
pub enum OpType {
    Binary,
    Prefix,
}

impl Compiler {
    pub fn new() -> Self {
        let mut constants = HashMap::new();
        constants.insert(().into(), 0);
        Self { constants, const_vec: Vec::new() }
    }

    pub(crate) fn fill_const_vec(&mut self) {
        let mut x: Vec<(Object, usize)> = self
            .constants
            .iter()
            .map(|(obj, num)| (obj.clone(), *num))
            .collect();
        x.sort_by_key(|k| k.1);
        self.const_vec.clear();
        for val in x {
            self.const_vec.push(val.0);
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
