use crate::common::*;
use std::{cell::RefCell, collections::HashMap, iter::once, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Enviroment {
  variables: Rc<RefCell<HashMap<String, Value>>>,
  stack:     Vec<Rc<RefCell<HashMap<String, Value>>>>,
}

// pub struct Enviroment {
//   variables: HashMap<String, Value>,
//   enclosing: Option<Rc<RefCell<Enviroment>>>,
// }

impl Enviroment {
  pub fn new() -> Enviroment {
    Enviroment {
      variables: Rc::new(RefCell::new(HashMap::new())),
      stack:     vec![],
    }
  }

  pub fn push_stack(&mut self) {
    self.stack.insert(
      0,
      Rc::new(RefCell::new(self.variables.replace(HashMap::new()))),
    );
  }

  pub fn pop_stack(&mut self) { self.variables = self.stack.remove(0); }

  pub fn has(&self, ident: &String) -> bool { self.variables.borrow().contains_key(ident) }

  pub fn get_from(&self, index: usize, ident: &String) -> Option<Value> {
    println!("{:?}", self);
    if index != 0 {
      self.stack[index - 1].borrow().get(ident).map(|x| x.clone())
    } else {
      self.variables.borrow().get(ident).map(|x| x.clone())
    }
  }

  fn resolve(&self, ident: &String) -> Option<usize> {
    let iter = once(&self.variables).chain(self.stack.iter()).enumerate();
    for (i, variables) in iter {
      if variables.borrow().contains_key(ident) {
        return Some(i);
      }
    }
    None
  }

  pub fn get(&self, ident: &String) -> Option<Value> {
    let iter = once(&self.variables).chain(self.stack.iter());
    for variables in iter {
      if let Some(value) = variables.borrow().get(ident) {
        return Some(value.clone());
      }
    }
    None
  }

  pub fn define(&mut self, ident: String, val: Value) {
    self.variables.borrow_mut().insert(ident, val);
  }

  pub fn set(&mut self, ident: String, val: Value) -> Result<(), String> {
    let iter = once(&mut self.variables).chain(self.stack.iter_mut());
    for variables in iter {
      if let Some(value) = variables.borrow_mut().get_mut(&ident) {
        *value = val;
        return Ok(());
      }
    }

    Err(format!("Variable {} is not declared in this scope", ident))
  }
}

#[macro_export]
macro_rules! scoped {
  ($env: ident, $expr: expr) => {{
    $env.push_stack();
    let res = $expr;
    $env.pop_stack();
    res
  }};
}
