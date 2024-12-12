use std::{collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn from_outer(outer: Rc<Environment>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        if let Some(local) = self.store.get(name) {
            Some(local)
        } else if let Some(outer) = &self.outer {
            outer.get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
