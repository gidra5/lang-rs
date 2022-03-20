use crate::{common::*, map};
use std::{cell::RefCell, collections::HashMap, iter::once, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Enviroment {
  variables: Rc<RefCell<HashMap<String, Value>>>,
  stack:     Vec<Rc<RefCell<HashMap<String, Value>>>>,
}

pub enum CloseBy {
  Copy,
  Ref,
  Move,
}

impl Enviroment {
  pub fn new() -> Enviroment { Enviroment::new_with_variables(map![]) }

  pub fn new_with_variables(variables: HashMap<String, Value>) -> Enviroment {
    Enviroment {
      variables: Rc::new(RefCell::new(variables)),
      stack:     vec![],
    }
  }

  pub fn push_stack(&mut self) {
    self
      .stack
      .insert(0, Rc::new(RefCell::new(self.variables.replace(map![]))));
  }

  pub fn pop_stack(&mut self) { self.variables = self.stack.remove(0); }

  pub fn has(&self, ident: &String) -> bool { self.variables.borrow().contains_key(ident) }

  pub fn get_from(&self, index: usize, ident: &String) -> Option<Value> {
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

  pub fn delete(&mut self, ident: &String) -> Option<Value> {
    let iter = once(&self.variables).chain(self.stack.iter());
    for variables in iter {
      if let Some(value) = variables.borrow_mut().remove(ident) {
        return Some(value);
      }
    }
    None
  }

  pub fn delete_from(&mut self, index: usize, ident: &String) -> Option<Value> {
    if index != 0 {
      self.stack[index - 1].borrow_mut().remove(ident)
    } else {
      self.variables.borrow_mut().remove(ident)
    }
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

  pub fn close_over_env(&mut self, vars: Vec<(CloseBy, String)>) -> Enviroment {
    let mut variables = map![];

    for (close_by, name) in vars {
      match close_by {
        CloseBy::Copy => match self.get(&name) {
          Some(val) => {
            variables.insert(name, val);
          },
          _ => (),
        },
        CloseBy::Ref => match self.delete(&name) {
          Some(val) => {
            let val_ref = Rc::new(RefCell::new(val));
            variables.insert(name.clone(), Value::Ref(val_ref.clone()));
            self.define(name, Value::Ref(val_ref));
          },
          _ => (),
        },
        CloseBy::Move => match self.delete(&name) {
          Some(val) => {
            variables.insert(name, val);
          },
          _ => (),
        },
      }
    }

    Enviroment::new_with_variables(variables)
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
