use std::{
  collections::HashMap,
  rc::{Rc, Weak},
};

use crate::{ast::Precedence, types::Type};

#[derive(Clone, Debug)]
pub struct Namespace(pub HashMap<String, Declaration>);

#[derive(Clone, Debug)]
pub enum Declaration {
  Variable(Type, Precedence),
  Namespace(Rc<Namespace>),
  ImportedNamespace(Weak<Namespace>),
}

impl Namespace {
  pub fn declare(&mut self, name: String, decl_type: Type) {
    self.declare_with_precedence(name, decl_type, Precedence(None, None));
  }

  pub fn declare_with_precedence(&mut self, name: String, decl_type: Type, precedence: Precedence) {
    self
      .0
      .insert(name, Declaration::Variable(decl_type, precedence));
  }

  pub fn declare_namespace(&mut self, name: String, namespace: Rc<Namespace>) {
    self.0.insert(name, Declaration::Namespace(namespace));
  }

  pub fn get(&self, name: &String) -> Option<Declaration> { self.0.get(name).cloned() }

  pub fn get_by_path(&self, path: Vec<String>) -> Option<Declaration> {
    let mut path_iter = path.into_iter();
    let mut item = None;

    let name = path_iter.next();
    if name.is_none() {
      return item;
    }

    let name = name.unwrap();
    item = self.get(&name);
    let mut namespace = if let Some(Declaration::Namespace(ref n)) = item {
      Rc::clone(n)
    } else if let Some(Declaration::ImportedNamespace(ref item)) = item {
      if let Some(n) = item.upgrade() {
        n
      } else {
        unreachable!()
      }
    } else {
      if path_iter.peekable().peek().is_none() || item.is_none() {
        return item;
      } else {
        unreachable!()
      }
    };

    loop {
      let name = path_iter.next();
      if name.is_none() || item.is_none() {
        break item;
      }

      let name = name.unwrap();
      item = namespace.get(&name);

      if let Some(Declaration::Namespace(ref n)) = item {
        namespace = Rc::clone(n)
      } else if let Some(Declaration::ImportedNamespace(ref item)) = item {
        if let Some(n) = item.upgrade() {
          namespace = n
        }
      }
    }
  }
}
