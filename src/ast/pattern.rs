use std::{default, fmt::Display};

use itertools::Itertools;

use crate::{
  check_token,
  common::{value::Value, ReversableIterator},
  enviroment::Enviroment,
  match_token,
  parse_error,
  token::TokenStream,
  types::Type,
  unwrap_enum_safe,
};

use super::{Expression, Parseable, ParsingContext, ParsingError, Precedence};

#[derive(Clone, PartialEq, Debug)]
pub enum RecordPatternKey {
  None,
  Rest,
  Identifier(String),
  Value(PatternWithDefault),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordPatternItem {
  pub key:   RecordPatternKey,
  pub value: PatternWithDefault,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
  Bind {
    // probably could replace name with "reversable" expression, such that we can derive
    // expression for all identifiers in it by applying some inverse fn to initial matched
    // expression
    name:       String,
    mutable:    bool,
    precedence: Precedence,
    _type:      Option<Type>,
    condition:  Option<Expression>,
  },
  BindAll,
  Value(Value),
  Record(Vec<RecordPatternItem>),
}

impl Parseable for Pattern {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    Ok(match stream.peek() {
      match_token!(Identifier) => {
        let (mutable, name) = if let match_token!({ src }, Identifier) = stream.peek() {
          stream.next();
          if src != "mut" {
            (false, src)
          } else {
            let name = if let match_token!({ src }, Identifier) = stream.peek() {
              stream.next();
              src
            } else {
              return Err(parse_error!("Expected identifier"));
            };

            (true, name)
          }
        } else {
          unreachable!()
        };

        let precedence = parse_precedence(stream, context)?;
        let _type = None;
        let condition = None;

        Pattern::Bind {
          name,
          mutable,
          precedence,
          _type,
          condition,
        }
      },
      _ => Self::from_expr(&Expression::parse(stream, context)?)?,
    })
  }
}

impl Pattern {
  pub fn from_expr(expr: &Expression) -> Result<Self, ParsingError> { todo!() }
  pub fn _match(&self, val: Value, env: &mut Enviroment) -> bool { todo!() }
  pub fn bind(&self, val: Value, env: &mut Enviroment) { todo!() }
  pub fn context_bind(&self, context: &mut ParsingContext, bound_expr: &Expression) { todo!() }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PatternWithDefault {
  pub pattern: Pattern,
  pub default: Option<Expression>,
}

impl Parseable for PatternWithDefault {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let pattern = Pattern::parse(stream, context)?;

    let default = if check_token!(stream.peek(), Equal) {
      stream.next();
      Some(Expression::parse(stream, context)?)
    } else {
      None
    };
    Ok(PatternWithDefault { pattern, default })
  }
}

impl PatternWithDefault {
  pub fn from_expr(expr: &Expression) -> Result<Self, ParsingError> { todo!() }
  pub fn _match(&self, val: Value, env: &mut Enviroment) -> bool { todo!() }
  pub fn bind(&self, val: Value, env: &mut Enviroment) { todo!() }
  pub fn context_bind(&self, context: &mut ParsingContext, bound_expr: &Expression) { todo!() }
}

impl Display for PatternWithDefault {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use self::Pattern::*;
    let pattern = match &self.pattern {
      Bind {
        name       ,
        ..
        // mutable    ,
        // implicit   ,
        // precedence ,
        // _type      ,
        // condition  ,
      } => format!("{}", name),
      BindAll => format!("*"),
      Value(v) => format!( "{}", v),
      Record(record_items) => {
        let mut x = record_items
          .iter()
          .map(|RecordPatternItem{key, value}| match key {
            RecordPatternKey::None => format!("{}", value),
            RecordPatternKey::Rest => format!("...{}", value),
            RecordPatternKey::Identifier(key) => format!("{}: {}", key, value),
            RecordPatternKey::Value(pat) => format!("[{}]: {}", pat, value),
          })
          .join(", ");
        if x.len() == 1 {
          format!( "{}", x)
        } else {
          format!( "({})", x)
        }
      },
    };

    if let Some(def) = &self.default {
      write!(f, "{} = {}", pattern, def)
    } else {
      write!(f, "{}", pattern)
    }
  }
}

fn parse_precedence(
  stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Precedence, ParsingError> {
  Ok((None, None))
}
