#![allow(unused)]
use crate::ast::Program;

#[path = "tests/evaluator.rs"]
mod tests;

pub struct Evaluator {}

impl Evaluator {
  pub fn evaluate(ast: Program) -> f64 {
    todo!();
  }
}
