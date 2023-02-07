use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

use crate::EvalObj;

#[derive(Debug, Default)]
pub struct EnvWrapper(pub HashMap<String, EvalObj>);
impl EnvWrapper {
    pub fn new_from_map(map: HashMap<String, EvalObj>) -> Self {
        Self(map)
    }
}
impl Deref for EnvWrapper {
    type Target = HashMap<String, EvalObj>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for EnvWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct Environment {
    pub env: Mutex<EnvWrapper>,
    pub outer: Option<Arc<Environment>>,
}
impl PartialEq for Environment {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
impl Default for Environment {
    fn default() -> Self {
        Self {
            env: Mutex::new(EnvWrapper::default()),
            outer: None,
        }
    }
}
impl Environment {
    pub fn new_from_map(map: HashMap<String, EvalObj>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: None,
        }
    }

    pub fn new_from_map_and_outer(map: HashMap<String, EvalObj>, outer: Arc<Environment>) -> Self {
        Self {
            env: Mutex::new(EnvWrapper::new_from_map(map)),
            outer: Some(outer),
        }
    }

    pub fn find(&self, key: &String) -> Option<EvalObj> {
        if !self.env.lock().unwrap().contains_key(key) {
            match &self.outer {
                Some(outer) => outer.find(key),
                None => None,
            }
        } else {
            self.env.lock().unwrap().get(key).cloned()
        }
    }

    pub fn set(&self, key: String, value: EvalObj) {
        self.env.lock().unwrap().insert(key, value);
    }

    pub fn has(&self, key: &String) -> bool {
        if !self.env.lock().unwrap().contains_key(key) {
            match &self.outer {
                Some(outer) => outer.has(key),
                None => false,
            }
        } else {
            true
        }
    }
}
