use crate::common::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Enviroment {
  variables: HashMap<String, Value>,
  enclosing: Option<Rc<RefCell<Enviroment>>>,
}

impl Enviroment {
  pub fn set_enclosing(&mut self, enclosing: Rc<RefCell<Enviroment>>) {
    self.enclosing = Some(enclosing);
  }

  pub fn new() -> Enviroment {
    Enviroment {
      variables: HashMap::new(),
      enclosing: None,
    }
  }

  fn has(&self, ident: &String) -> bool { matches!(self.variables.get(ident), Some(_)) }

  pub fn get(&self, ident: String) -> Option<Value> {
    Some(match self.variables.get(&ident) {
      Some(val) => val.clone(),
      None => self.enclosing.as_ref()?.borrow().get(ident)?,
    })
  }

  pub fn define(&mut self, ident: String, val: Value) { self.variables.insert(ident, val); }

  pub fn set(&mut self, ident: String, val: Value) -> Result<(), String> {
    Ok(match &self.enclosing {
      Some(enclosing) => {
        enclosing.borrow_mut().set(ident, val)?;
      },
      None => {
        if self.has(&ident) {
          self.variables.insert(ident, val);
        } else {
          return Err(format!("Variable {} is not declared in this scope", ident));
        }
      },
    })
  }
}
