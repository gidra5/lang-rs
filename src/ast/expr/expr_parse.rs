use std::cmp::Ordering;

use crate::{
  ast::{Operator, Parseable, ParsingContext, ParsingError},
  check_token,
  check_token_end,
  common::{ReversableIterator, Span},
  match_token,
  parse_error,
  punct_or_newline,
  skip,
  token::{Token, TokenExt, TokenStream},
  token_pat,
  types::Declaration,
};

use super::expr_struct::{Expression, RecordItem, RecordKey};



#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}


pub fn parse_braces(
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

pub fn parse_expr(
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
        None => top
          .lhs
          .ok_or(parse_error!("Unexpected indexing position"))?,
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
          context,
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
            operator2 = operator2.map(|x| x.convert(context));
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
