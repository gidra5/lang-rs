use crate::{
  check_token,
  common::{value::Value, ReversableIterator},
  parse_error,
  token::TokenStream,
  token_pat,
};

use super::{Expression, Parseable, ParsingContext, ParsingError, Pattern};

#[derive(Clone, Debug, PartialEq)]
pub struct Import(pub String, Option<Pattern>);

impl Parseable for Import {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    Ok(
      if check_token!(stream.next(), Import) {
        if let Some(Value::String(import_path)) = stream.next().map(|x| x.value()) {
          if check_token!(stream.peek(), As) {
            stream.next();
            Self(import_path, Some(Pattern::parse(stream, context)?))
          } else {
            Self(import_path, None)
          }
        } else {
          return Err(parse_error!("Expected string representing path to module"));
        }
      } else {
        return Err(parse_error!("Expected 'use' keyword"));
      },
    )
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExternalDependency(pub String, Expression);

impl Parseable for ExternalDependency {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    Ok(
      if check_token!(stream.next(), External) {
        if let Some(token_pat!(token: Identifier, src)) = stream.next() {
          let ident = src;
          if check_token!(stream.next(), As) {
            stream.next();
            Self(ident, Expression::parse(stream, context)?)
          } else {
            return Err(parse_error!("Expected explicit type for external variable"));
          }
        } else {
          return Err(parse_error!("Expected an identifier"));
        }
      } else {
        return Err(parse_error!("Expected 'ext' keyword"));
      },
    )
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
  name:  String,
  expr:  Expression,
  _type: Option<Expression>,
}

impl Parseable for Declaration {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    todo!()
  }
}
