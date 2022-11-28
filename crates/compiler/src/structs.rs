use std::{collections::HashMap, fmt::Debug};

use bytecode::OpCode;
use object::Object;

pub use crate::symbol_table::Symbol;
use crate::symbol_table::SymbolTable;

pub struct Compiler {
    pub constants: HashMap<Object<()>, usize>,
    pub bytecode: Vec<OpCode>,
    pub(crate) symbol_table: SymbolTable,
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
}

impl Compiler {
    pub fn new() -> Self {
        let mut constants = HashMap::new();
        constants.insert(().into(), 0);
        Self {
            constants,
            bytecode: Vec::new(),
            symbol_table: SymbolTable::default(),
        }
    }

    pub fn get_fields(self) -> (Vec<OpCode>, Vec<Object<()>>) {
        let mut x: Vec<(Object<()>, usize)> = self.constants.into_iter().collect();
        x.sort_by_key(|k| k.1);
        let mut constants = Vec::new();
        for val in x {
            constants.push(val.0);
        }
        (self.bytecode, constants)
    }
}
impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
