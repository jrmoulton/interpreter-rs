use std::{collections::HashMap, rc::Rc};

const GLOBAL_SCOPE: u32 = 0;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol {
    name: Rc<str>,
    pub(crate) scope: u32,
    pub(crate) index: usize,
}

#[derive(Debug, Default)]
pub(crate) struct SymbolTable {
    pub(crate) table: HashMap<Rc<str>, Symbol>,
}
impl IntoIterator for SymbolTable {
    type IntoIter = std::collections::hash_map::IntoIter<Rc<str>, Symbol>;
    type Item = (Rc<str>, Symbol);

    fn into_iter(self) -> Self::IntoIter {
        self.table.into_iter()
    }
}
impl SymbolTable {
    pub(crate) fn define(&mut self, name: Rc<str>) {
        let curr_idx = self.table.len();
        let symbol = Symbol {
            name: name.clone(),
            scope: GLOBAL_SCOPE,
            index: curr_idx,
        };
        self.table.insert(name, symbol);
    }

    pub(crate) fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.table.get(name)
    }
}
