use crate::{
  check_token_end,
  common::ReversableIterator,
  match_token,
  parse_error,
  skip,
  token::TokenStream,
  token_pat,
};

use super::{
  Declaration,
  Expression,
  ExternalDependency,
  Import,
  Parseable,
  ParsingContext,
  ParsingError,
};

#[path = "../tests/module.rs"]
mod tests;

#[derive(Clone, Debug, PartialEq)]
pub enum ModuleItem {
  Import(Import),
  External(ExternalDependency),
  Module(String, Module),
  Declaration(Declaration),
}

impl Parseable for ModuleItem {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    Ok(match stream.peek() {
      match_token!(Import) => Self::Import(Import::parse(stream, context)?),
      match_token!(External) => Self::External(ExternalDependency::parse(stream, context)?),
      match_token!(Module) => {
        stream.next();
        if let match_token!({ src }, Identifier) = stream.next() {
          skip!(stream, LBracket);
          let module = Module::parse(stream, context)?;
          skip!(stream, RBracket);
          Self::Module(src, module)
        } else {
          return Err(parse_error!("Expected identifier after 'mod' keyword"));
        }
      },
      Some(token_pat!(token: Identifier, src)) if src == "let" => {
        Self::Declaration(Declaration::parse(stream, context)?)
      },
      _ => return Err(parse_error!("Unrecognized module item")),
    })
  }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Module {
  pub _pub:    Vec<ModuleItem>,
  pub private: Vec<ModuleItem>,
  pub entry:   Option<(Option<String>, Expression)>,
}

impl Parseable for Module {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let mut _pub = vec![];
    let mut private = vec![];
    let mut entry = None;
    let mut errors = vec![];

    loop {
      if check_token_end!(stream) {
        if errors.len() == 0 {
          return Ok(Self {
            _pub,
            private,
            entry,
          });
        } else {
          return Err(ParsingError::Aggregate(errors));
        }
      }

      match stream.peek() {
        Some(token_pat!(token: Identifier, src)) if src == "let" => {
          match ModuleItem::parse(stream, context) {
            Ok(item) => private.push(item),
            Err(err) => errors.push(err),
          };
        },
        match_token!(Import | External) => {
          match ModuleItem::parse(stream, context) {
            Ok(item) => private.push(item),
            Err(err) => errors.push(err),
          };
        },
        match_token!(Public) => {
          stream.next();
          match ModuleItem::parse(stream, context) {
            Ok(item) => _pub.push(item),
            Err(err) => errors.push(err),
          };
        },
        match_token!(Entry) => {
          stream.next();

          match &stream.peek_ext(2)[..] {
            [match_token!({ src }, Equal), match_token!(Equal)] => {
              stream.next_ext(2);
              if entry.is_some() {
                return Err(parse_error!("Entry point was already defined"));
              } else {
                entry = Some((Some(src.clone()), Expression::parse(stream, context)?))
              }
            },
            [match_token!(Equal), _] => {
              stream.next_ext(2);
              if entry.is_some() {
                return Err(parse_error!("Entry point was already defined"));
              } else {
                entry = Some((None, Expression::parse(stream, context)?))
              }
            },
            _ => return Err(parse_error!("Unexpected keyword 'entry'")),
          };
        },
        _ => (),
      }

      skip!(stream, Semicolon | NewLine);
    }
  }
}
