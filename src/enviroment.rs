use crate::common::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Enviroment {
  variables: HashMap<String, Value>,
  enclosing: Option<Box<Enviroment>>,
}

impl Enviroment {
  pub fn new() -> Enviroment {
    Enviroment {
      variables: HashMap::new(),
      enclosing: None,
    }
  }

  pub fn get(&self, ident: String) -> Option<Value> {
    Some(match self.variables.get(&ident) {
      Some(val) => val.clone(),
      None => self.enclosing.as_ref()?.get(ident)?,
    })
  }
  pub fn set(&mut self, ident: String, val: Value) { self.variables.insert(ident, val); }
}
