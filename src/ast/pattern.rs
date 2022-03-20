use crate::{common::value::Value, enviroment::Enviroment, token::TokenStream, types::Type};

use super::{Expression, Parseable, ParsingContext, ParsingError, Precedence};

#[derive(Clone, PartialEq, Debug)]
pub enum RecordPatternKey {
  None,
  Rest,
  Identifier(String),
  Value(Pattern),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordPatternItem {
  pub key:   RecordPatternKey,
  pub value: Pattern,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
  Bind {
    // probably could replace name with "reversable" expression, such that we can derive
    // expression for all identifiers in it by applying inverse transformation to initial matched
    // expression
    name:       String,
    mutable:    bool,
    reference:  bool,
    implicit:   bool,
    precedence: Precedence,
    _type:      Option<Type>,
    condition:  Option<Expression>,
    default:    Option<Expression>,
  },
  Value(Value),
  Ref(Box<Pattern>),
  EnumValue(String, Box<Pattern>),
  Record(Vec<RecordPatternItem>),
}

impl Parseable for Pattern {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    todo!()
  }
}

impl Pattern {
  pub fn from_expr(expr: Expression) -> Pattern { todo!() }
  pub fn is_matching(&self, val: Value, env: &mut Enviroment) -> bool { todo!() }
  pub fn bind(&self, val: Value, env: &mut Enviroment) { todo!() }
  pub fn context_bind(&self, context: &mut ParsingContext, bound_expr: &Expression) { todo!() }
}
