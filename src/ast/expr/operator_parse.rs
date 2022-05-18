use crate::{
  common::Buffered,
  errors::ParsingError,
  is_next,
  parse_error,
  parseable::ParsingContext,
  token::Token,
};

use super::Operator;

#[macro_export]
macro_rules! get_number {
  ($token_stream: ident, $def:literal) => {
    $token_stream
      .next()
      .and_then(|x| {
        if let token_pat!(token: Number, src) = x {
          Some(src.parse::<u8>().unwrap())
        } else {
          None
        }
      })
      .unwrap_or($def)
  };
}

/*

*/

#[macro_export]
macro_rules! parse_operand {
  ($errors: expr, $iter: expr, $ctx: expr) => {{
    use crate::{ast::Operator, parse_error, token::Token};
    match $iter.peek() {
      Some(Token::For) => Operator::parse(
        vec![Token::For, Token::In, Token::Colon],
        $iter,
        $errors,
        $ctx,
      ),
      Some(Token::If) => Operator::parse(vec![Token::If, Token::Colon], $iter, $errors, $ctx),
      Some(Token::LParenthesis) => Operator::parse(
        vec![Token::LParenthesis, Token::RParenthesis],
        $iter,
        $errors,
        $ctx,
      ),
      Some(Token::RParenthesis) => {
        $errors.push(parse_error!("Unexpected closing parenthesis"));
        None
      },
      Some(Token::LBrace) => {
        Operator::parse(vec![Token::LBrace, Token::RBrace], $iter, $errors, $ctx)
      },
      Some(Token::LBracket) => {
        Operator::parse(vec![Token::LBracket, Token::RBracket], $iter, $errors, $ctx)
      },
      Some(token) => {
        $iter.next();
        Some(Operator::Token(token))
      },
      None => {
        $errors.push(parse_error!("Unexpected end of expression"));
        None
      },
    }
  }};
}

impl Operator {
  pub fn parse<T: Iterator<Item = Token>>(
    op: Vec<Token>,
    tokens: &mut Buffered<T>,
    errors: &mut Vec<ParsingError>,
    context: &mut ParsingContext,
  ) -> Option<Operator> {
    if let Some(Token::NewLine) = tokens.peek() {
      tokens.next();
    }
    let mut operands = vec![];
    let mut iter = op.iter();

    if let Some(token) = iter.next() {
      match tokens.next() {
        Some(ref t) if t != token => {
          errors.push(parse_error!("Unexpected end of expression"));
          return None;
        },
        _ => (),
      }
    }

    for token in iter {
      let mut operand = vec![];
      while tokens.peek().is_some_with(|x| x != token) {
        is_next!([skip] tokens, Token::NewLine);

        let res = parse_operand!(errors, tokens, context);
        if let Some(t) = res {
          operand.push(t);
        } else {
          errors.push(parse_error!("Unexpected end of expression"));
          return None;
        }
      }
      if tokens.next().is_none() {
        errors.push(parse_error!("Unexpected end of expression"));
        return None;
      }
      operands.push(operand);
    }

    Some(Operator::Operand { operands, op })
  }
}
