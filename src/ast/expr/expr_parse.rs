use std::cmp::Ordering;

use crate::{
  ast::{Parseable, ParsingContext, ParsingError, Statement},
  check_token,
  check_token_end,
  common::{ReversableIterator, Span},
  match_token,
  parse_error,
  punct_or_newline,
  skip,
  token::{Token, TokenExt, TokenStream},
  token_pat,
};

use super::expr_struct::{Expression, RecordItem, RecordKey};

// #[derive(Clone, PartialEq, Default, Eq, Debug)]
// pub enum Fixity {
//   Infix(l_bp, r_bp),
//   Prefix(r_bp),
//   Postfix(l_pb),

//   #[default]
//   None,
// }

#[derive(Clone, PartialEq, Default, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,

  #[default]
  None,
}

pub type Precedence = Option<(Option<u8>, Option<u8>)>;

#[derive(PartialEq, Default, Debug, Clone)]
pub struct Operator {
  op:         Expression,
  precedence: Precedence,
}


#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

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
    .map_err(|err| parse_error!("Can't parse true branch: {}", err))?;

  if true_block == Expression::default() {
    return Err(parse_error!("Empty true branch in if expression"));
  }
  skip!(token_stream, NewLine);

  let false_block = if check_token!(token_stream.peek(), Else) {
    token_stream.next();
    skip!(token_stream, NewLine);

    let false_block = Expression::parse(token_stream, context)
      .map_err(|err| parse_error!("Can't parse false branch: {}", err))?;

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

fn parse_braces(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
) -> Result<Expression, ParsingError> {
  let expr = Expression::parse(token_stream, context)?;

  if !check_token!(token_stream.next(), RBrace) {
    Err(parse_error!("Expected closing brace"))
  } else {
    Ok(expr)
  }
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
      Ok(expr) => res.push(Statement::Expression(expr)),
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

fn parse_expr(
  token_stream: &mut TokenStream,
  context: &mut ParsingContext,
  in_parens: bool,
) -> Result<Expression, ParsingError> {
  let mut top = Frame {
    lhs:      None,
    operator: None,
  };
  let mut stack = Vec::new();

  loop {
    if in_parens {
      skip!(token_stream, NewLine);
    } else if let match_token!(RParenthesis) = token_stream.peek() {
      return Err(parse_error!("Unexpected closing parenthesis"));
    }

    let token = token_stream.peek();

    if let match_token!(LBrace) = token {
      token_stream.next();

      let operand = match stack.pop() {
        None => {
          top
            .lhs
            .ok_or(parse_error!("Unexpected indexing position"))?
        },
        Some(Frame { lhs, operator }) => {
          let op = top
            .operator
            .ok_or(parse_error!("Unexpected indexing position"))?
            .op;
          top.operator = operator;

          Expression::from_options(op, lhs, top.lhs)
        },
      };
      top.lhs = Some(Expression::Infix {
        left:  Box::new(operand),
        op:    Box::new(Expression::Value(token.unwrap())),
        right: Box::new(parse_braces(token_stream, context)?),
      });
      continue;
    }

    let mut operator2 = Operator::parse(token_stream, context, top.lhs.is_none());

    let operator = loop {
      if matches!(token, match_token!(For | If | LParenthesis | LBracket)) {
        // println!("{:?} {:?}", operator2, top.operator, );
        break operator2?;
      }

      let operator = operator2
        .as_ref()
        .ok()
        .and_then(|x| x.clone().ensure_exists());

      if matches!(
        operator.clone().map(|x| x.precedence),
        Some(Some((None, None)))
      ) && matches!(
        top.operator.clone().map(|x| x.precedence),
        Some(Some((None, None)))
      ) {
        let operator = Some(Operator::new(
          TokenExt {
            token: Token::Apply,
            src:   "apply".to_string(),
            span:  Span::default(),
          },
          false,
        ));

        loop {
          let res = top;

          top = match stack.pop() {
            Some(it) => it,
            None => return Ok(res.lhs.unwrap_or_default()),
          };

          let op = res.operator.unwrap().op;

          top.lhs = Some(
            if let Expression::Value(token_pat!(token: Apply)) = op {
              Expression::Prefix {
                op:    Box::new(top.lhs.unwrap()),
                right: Box::new(res.lhs.unwrap()),
              }
            } else {
              Expression::from_options(op, top.lhs, res.lhs)
            },
          );

          if top.operator <= operator {
            break;
          }
        }

        stack.push(top);
        top = Frame {
          lhs: None,
          operator,
        };

        continue;
      }

      match operator {
        Some(op) if top.operator <= Some(op.clone()) => {
          token_stream.next();
          break op;
        },
        x => {
          // println!("2 ===\n\n{:?}\n\n{:?}\n\n{:?}", stack, top, x);

          let res = top;

          top = match stack.pop() {
            Some(it) => it,
            None => return Ok(res.lhs.unwrap_or_default()),
          };

          let op = res.operator.unwrap().op;

          top.lhs = Some(
            if let Expression::Value(token_pat!(token: Apply)) = op {
              Expression::Prefix {
                op:    Box::new(top.lhs.unwrap()),
                right: Box::new(res.lhs.unwrap()),
              }
            } else {
              Expression::from_options(op, top.lhs, res.lhs)
            },
          );

          if !matches!(
            operator2.as_ref().map(|x| x.precedence),
            Ok(Some((Some(_), _)))
          ) {
            operator2 = operator2.map(|x| x.convert());
          }
        },
      };
    };

    stack.push(top);
    top = Frame {
      lhs:      None,
      operator: Some(operator),
    };
  }
}

impl Parseable for Expression {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    parse_expr(stream, context, false)
  }
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
  fn parse(
    token_stream: &mut TokenStream,
    context: &mut ParsingContext,
    prefix: bool,
  ) -> Result<Self, ParsingError> {
    let precedence = Some(match token_stream.peek() {
      match_token!(Infix) => {
        token_stream.next();
        (
          Some(get_number!(token_stream, 127)),
          Some(get_number!(token_stream, 127)),
        )
      },
      match_token!(Prefix) => {
        token_stream.next();

        (None, Some(get_number!(token_stream, 127)))
      },
      match_token!(Postfix) => {
        token_stream.next();
        (Some(get_number!(token_stream, 127)), None)
      },
      _ => (None, None),
    });

    if prefix && matches!(precedence, Some((Some(_), Some(_)))) {
      Err(parse_error!("Can't use infix operator in prefix position"))
    } else if prefix && matches!(precedence, Some((Some(_), None))) {
      Err(parse_error!(
        "Can't use postfix operator in prefix position"
      ))
    } else if !prefix && matches!(precedence, Some((None, Some(_)))) {
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
          return Ok(Operator::new(token, prefix));
        },
        None => return Err(parse_error!("Unexpected end of expression")),
      };

      Ok(Operator { op, precedence })
    }
  }

  fn ensure_exists(self) -> Option<Self> {
    Some(
      if self.precedence == None {
        return None;
      } else {
        self
      },
    )
  }

  fn convert(self) -> Self {
    match self {
      Self {
        op: Expression::Value(token),
        ..
      } => Self::new(token, false),
      x => x,
    }
  }

  pub fn new(token: TokenExt, prefix: bool) -> Self {
    let precedence = Self::get_precedence(
      &token,
      if prefix {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    )
    .or_else(|| {
      Self::get_precedence(
        &token,
        if prefix {
          Fixity::None
        } else {
          Fixity::Postfix
        },
      )
    });

    Self {
      precedence,
      op: Expression::Value(token),
    }
  }

  fn get_precedence(token: &TokenExt, fixity: Fixity) -> Precedence {
    Some(match (&token, &fixity) {
      (
        token_pat!(token: Identifier | String | Placeholder | Char | Number | Boolean),
        Fixity::None,
      ) => (None, None),
      (token_pat!(token, src), Fixity::Prefix) => {
        (
          None,
          Some(match token {
            Token::Add | Token::Sub => 9,
            Token::Inc | Token::Dec => 11,
            Token::Mult => 13,
            Token::Identifier if src == "not" => 29,
            Token::Async => 36,
            Token::Await => 33,
            Token::Inline => 36,
            Token::Identifier if src == "print" => 0,
            Token::Identifier if src == "let" => 98,
            _ => return None,
          }),
        )
      },
      (token_pat!(token, src), Fixity::Postfix) => {
        (
          Some(match token {
            Token::Bang => 15,
            _ => return None,
          }),
          None,
        )
      },
      (token_pat!(token, src), Fixity::Infix) => {
        match token {
          Token::LBrace => (Some(26), Some(27)),
          Token::Period => (Some(24), Some(23)),
          Token::Equal => (Some(97), Some(1)),
          Token::Mod => (Some(28), Some(29)),
          Token::Add | Token::Sub => (Some(5), Some(6)),
          Token::Mult | Token::Div => (Some(7), Some(8)),
          Token::EqualEqual => (Some(20), Some(19)),
          Token::LAngleBracket => (Some(20), Some(19)),
          Token::RAngleBracket => (Some(20), Some(19)),
          Token::LessEqual => (Some(20), Some(19)),
          Token::GreaterEqual => (Some(20), Some(19)),
          Token::Arrow => (Some(37), Some(0)),
          Token::Apply => (Some(34), Some(35)),
          Token::Is => (Some(32), Some(33)),
          Token::Hash => (Some(97), Some(97)),

          Token::Identifier if src == "mod" => (Some(22), Some(21)),
          Token::Identifier if src == "and" => (Some(24), Some(23)),
          Token::Identifier if src == "or" => (Some(25), Some(26)),
          _ => return None,
        }
      },
      _ => return None,
    })
  }
}

impl PartialOrd for Operator {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    // aka r_bp1 < l_bp2
    Some(match (self.precedence, other.precedence) {
      (Some((_, r_bp1)), Some((l_bp2, _))) => {
        match (r_bp1, l_bp2) {
          (None, None) => Ordering::Greater,
          (Some(_), None) => Ordering::Less,
          (None, Some(_)) => Ordering::Less,
          (Some(r_bp1), Some(l_bp2)) => {
            match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
              (false, false) => Ordering::Equal,
              (true, false) => Ordering::Less,
              (false, true) => Ordering::Greater,
              _ => return None,
            }
          },
        }
      },
      (x, y) => x.partial_cmp(&y)?,
    })
  }
}
