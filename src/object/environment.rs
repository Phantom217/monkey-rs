use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

pub type MutEnv = Rc<RefCell<Environment>>;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> MutEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
        }))
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: &str, val: Object) -> Option<Object> {
        self.store.insert(name.to_string(), val)
    }
}
