use crate::ast::Expression;

use super::str_parse;

pub fn expr(input: &str) -> Result<Expression, String> { str_parse::<Expression>(input) }
