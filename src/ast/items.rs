use crate::{
  check_token,
  common::{value::Value, ReversableIterator},
  parse_error,
  token::TokenStream,
  token_pat,
  unwrap_enum_safe,
};

use super::{Expression, Parseable, ParsingContext, ParsingError, Pattern, PatternWithDefault};

#[derive(Clone, Debug, PartialEq)]
pub struct Import(pub String, pub Option<Pattern>, pub Option<Expression>);

impl Parseable for Import {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    if !check_token!(stream.next(), Import) {
      return Err(parse_error!("Expected 'use' keyword"));
    }
    let import_path = unwrap_enum_safe!(
      stream
        .next()
        .ok_or(parse_error!("Expected string representing path to module"))?
        .value(),
      Value::String
    )
    .ok_or(parse_error!("Expected string representing path to module"))?;
    let binding = if check_token!(stream.peek(), As) {
      stream.next();
      Some(Pattern::parse(stream, context)?)
    } else {
      None
    };
    let with_expr = if check_token!(stream.peek(), With) {
      stream.next();
      Some(Expression::parse(stream, context)?)
    } else {
      None
    };

    Ok(Self(import_path, binding, with_expr))
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExternalDependency(pub String, Expression);

impl Parseable for ExternalDependency {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    if !check_token!(stream.next(), External) {
      return Err(parse_error!("Expected 'ext' keyword"));
    }

    let ident = if let Some(token_pat!(token: Identifier, src)) = stream.next() {
      src
    } else {
      return Err(parse_error!("Expected an identifier"));
    };

    let _type = if check_token!(stream.next(), As) {
      Expression::parse(stream, context)?
    } else {
      return Err(parse_error!("Expected explicit type for external variable"));
    };

    Ok(Self(ident, _type))
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
  pat:  Pattern,
  expr: Expression,
}

impl Parseable for Declaration {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    stream
      .next()
      .and_then(|t| match t {
        token_pat!(token: Identifier, src) if src == "let" => Some(()),
        _ => None,
      })
      .ok_or(parse_error!("Expected 'let' keyword"))?;

    let pat = Pattern::parse(stream, context)?;

    let expr = if check_token!(stream.next(), Equal) {
      Expression::parse(stream, context)?
    } else {
      return Err(parse_error!("Expected '=' and initial value"));
    };

    Ok(Declaration { pat, expr })
  }
}
