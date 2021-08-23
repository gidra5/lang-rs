use crate::{check_token, check_token_end, common::char_stream::TokenStream, parse_stmt_vec};

use super::{stmt::Statement, Parseable};


#[derive(Clone, Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

impl<'a> Parseable<'a> for Program {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let res = parse_stmt_vec!(stream)?;

    Ok(Self(res))
  }
}
