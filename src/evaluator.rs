#![allow(unused)]
use crate::ast::*;
use crate::token::*;
use crate::common::*;
use std::collections::HashMap;
use futures::executor::block_on;

#[path = "tests/evaluator.rs"]
mod tests;

pub struct Evaluator {
  decls: HashMap<Identifier, Expression>
}

impl Evaluator {
  pub fn new() -> Self {
    Self {
      decls: HashMap::new(),
    }
  }

  pub fn evaluate(&mut self, ast: Program) {
    for line in ast.0 {
      match line {
        Either::Left(decl) => { self.decls.insert(decl.0.0, decl.1); },
        Either::Right(expr) => println!("{}", block_on(expr.evaluate(&self.decls))),
      }
    }
  }
}
