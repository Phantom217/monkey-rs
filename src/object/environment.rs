use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

pub type MutEnv = Rc<RefCell<Environment>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<MutEnv>,
}

impl Environment {
    pub fn new() -> MutEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: MutEnv) -> MutEnv {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, ident: &str) -> Option<Object> {
        match self.store.get(ident) {
            Some(object) => Some(object.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(ident)),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) -> Option<Object> {
        self.store.insert(name.to_string(), val)
    }
}
