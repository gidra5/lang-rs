use crate::{
  ast::{parse_expr, Expression, Parseable, ParsingContext, ParsingError, RecordItem, RecordKey},
  check_token,
  check_token_end,
  common::ReversableIterator,
  match_token,
  parse_error,
  punct_or_newline,
  skip,
  token::{Token, TokenStream},
  token_pat,
};

use super::{Fixity, Operator};


fn parse_for(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Expression, ParsingError> {
  let pat = Expression::parse(token_stream, context)?;

  if !check_token!(token_stream.next(), In) {
    return Err(parse_error!("Expected 'in' after pattern"));
  }

  let iterator = Expression::parse(token_stream, context)?;

  if !punct_or_newline!(token_stream.next(), Colon) {
    return Err(parse_error!(
      "Missing colon after expression in for statement"
    ));
  }

  let body = Expression::parse(token_stream, context)?;

  return Ok(Expression::For(
    Box::new(pat),
    Box::new(iterator),
    Box::new(body),
  ));
}

fn parse_if(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Expression, ParsingError> {
  let expr = Expression::parse(token_stream, context)?;

  if !punct_or_newline!(token_stream.next(), Colon) {
    return Err(parse_error!(
      "Missing colon after condition expression in an if expression"
    ));
  }

  let true_block = Expression::parse(token_stream, context)
    .map_err(|err| parse_error!("Can't parse true branch: {err}"))?;

  if true_block == Expression::default() {
    return Err(parse_error!("Empty true branch in if expression"));
  }
  skip!(token_stream, NewLine);

  let false_block = if check_token!(token_stream.peek(), Else) {
    token_stream.next();
    skip!(token_stream, NewLine);

    let false_block = Expression::parse(token_stream, context)
      .map_err(|err| parse_error!("Can't parse false branch: {err}"))?;

    if false_block == Expression::default() {
      return Err(parse_error!("Empty false branch in if expression"));
    }

    Some(Box::new(false_block))
  } else {
    None
  };

  return Ok(Expression::If(
    Box::new(expr),
    Box::new(true_block),
    false_block,
  ));
}

fn parse_block(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Expression, ParsingError> {
  let mut res = vec![];

  loop {
    if check_token!(token_stream.peek(), RBracket) {
      token_stream.next();
      break Ok(Expression::Block(res));
    } else if check_token_end!(token_stream) {
      break Err(parse_error!("Expected closing bracket"));
    }

    skip!(token_stream, Semicolon | NewLine);

    match Expression::parse(token_stream, context) {
      Ok(expr) if expr == Expression::default() => (),
      Ok(expr) => res.push(expr),
      Err(msg) => return Err(msg),
    };
  }
}

fn parse_parens(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Expression, ParsingError> {
  let mut x = vec![];

  if check_token!(token_stream.peek(), RParenthesis) {
    token_stream.next();

    return Ok(Expression::Record(x));
  }

  while !check_token!(token_stream.peek(), RParenthesis) {
    if check_token!(token_stream.peek(), NewLine) {
      token_stream.next();
      continue;
    }
    let y = token_stream.peek_ext(2);
    let mut key = RecordKey::None;


    if check_token!(y[0], Identifier) && check_token!(y[1], Colon) {
      key = RecordKey::Identifier(token_stream.next().unwrap().src);
      token_stream.next();
    } else if check_token!(y[0], LBrace) {
      token_stream.next();
      key = RecordKey::Value(parse_expr(token_stream, context, true)?);
      let y = token_stream.peek_ext(2);

      if !(check_token!(y[0], RBrace) && check_token!(y[1], Colon)) {
        return Err(parse_error!("Expected closing brace and colon"));
      } else {
        token_stream.next_ext(2);
      }
    }

    let expr = parse_expr(token_stream, context, true)?;

    skip!(token_stream, Comma);

    if check_token_end!(token_stream) {
      return Err(parse_error!("Unexpected end of input"));
    }

    x.push(RecordItem { key, value: expr });
  }
  token_stream.next();

  Ok(Expression::Record(x))
}

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

impl Operator {
  pub fn parse(
    token_stream: &mut TokenStream,
    context: &mut ParsingContext,
    prefix: bool,
  ) -> Result<Self, ParsingError> {
    let (prefix_expr, precedence) = match token_stream.peek() {
      match_token!(Infix) => {
        let t = token_stream.next().unwrap();
        let mut x = token_stream.peek_ext(2);
        let x2 = x
          .pop()
          .unwrap()
          .ok_or(parse_error!("Unexpected end of expression"))?;
        let x1 = x
          .pop()
          .unwrap()
          .ok_or(parse_error!("Unexpected end of expression"))?;

        (
          Some(Expression::Prefix {
            op:    Box::new(Expression::Value(t)),
            right: Box::new(Expression::Record(vec![
              RecordItem {
                key:   RecordKey::None,
                value: Expression::Value(x1),
              },
              RecordItem {
                key:   RecordKey::None,
                value: Expression::Value(x2),
              },
            ])),
          }),
          (
            Some(get_number!(token_stream, 127)),
            Some(get_number!(token_stream, 127)),
          ),
        )
      },
      match_token!(Prefix) => {
        let t = token_stream.next().unwrap();
        let x = token_stream
          .peek()
          .ok_or(parse_error!("Unexpected end of expression"))?;

        (
          Some(Expression::Prefix {
            op:    Box::new(Expression::Value(t)),
            right: Box::new(Expression::Value(x)),
          }),
          (None, Some(get_number!(token_stream, 127))),
        )
      },
      match_token!(Postfix) => {
        let t = token_stream.next().unwrap();
        let x = token_stream
          .peek()
          .ok_or(parse_error!("Unexpected end of expression"))?;

        (
          Some(Expression::Prefix {
            op:    Box::new(Expression::Value(t)),
            right: Box::new(Expression::Value(x)),
          }),
          (Some(get_number!(token_stream, 127)), None),
        )
      },
      _ => (None, (None, None)),
    };

    if prefix && matches!(precedence, ((Some(_), Some(_)))) {
      Err(parse_error!("Can't use infix operator in prefix position"))
    } else if prefix && matches!(precedence, ((Some(_), None))) {
      Err(parse_error!(
        "Can't use postfix operator in prefix position"
      ))
    } else if !prefix && matches!(precedence, ((None, Some(_)))) {
      Err(parse_error!(
        "Can't use prefix operator in non prefix position"
      ))
    } else {
      let op = match token_stream.next() {
        match_token!(For) => parse_for(token_stream, context)?,
        match_token!(If) => parse_if(token_stream, context)?,
        match_token!(LParenthesis) => parse_parens(token_stream, context)?,
        match_token!(LBracket) => parse_block(token_stream, context)?,
        Some(token) => {
          token_stream.backtrack(1);
          let op = Operator::new(token, context, prefix);
          return Ok(op);
        },
        None => return Err(parse_error!("Unexpected end of expression")),
      };
      let op = prefix_expr.map_or(op.clone(), |x| Expression::Prefix {
        op:    Box::new(x),
        right: Box::new(op),
      });
      let precedence = Some(precedence);

      Ok(Operator { op, precedence })
    }
  }
}
