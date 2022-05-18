use crate::{
  ast::{RecordItem, RecordKey},
  common::{buffered_iterator::Buffered, Buf},
  errors::ParsingError,
  parseable::{Parseable, ParsingContext},
  token::Token,
};
use itertools::Itertools;

use super::{Expression, Operator, Precedence};

#[derive(Debug, Clone, PartialEq)]
struct FrameOperator {
  operator:   Operator,
  precedence: Precedence,
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<FrameOperator>,
  lhs:      Option<Expression>,
}

pub struct ExpressionParsingInput<T: Iterator<Item = Operator>> {
  pub operands: Buffered<T>,
  pub context:  ParsingContext,
  pub errors:   Vec<ParsingError>,
}

impl<T: Iterator<Item = Operator>> Parseable<ExpressionParsingInput<T>> for Expression {
  fn parse(input: ExpressionParsingInput<T>) -> (ExpressionParsingInput<T>, Option<Self::O>) {
    let ExpressionParsingInput {
      mut operands,
      mut context,
      mut errors,
    } = input;
    let mut top = Frame {
      lhs:      None,
      operator: None,
    };
    let mut stack = Vec::new();

    loop {
      let mut operand = operands.peek();
      let mut skip = true;

      let operator = loop {
        let top_operator_precedence = top.operator.clone().map(|x| x.precedence);
        let precedence = operand
          .clone()
          .and_then(|operand| operand.get_precedence(&mut context, top.lhs.is_none()));
        let is_apply = matches!(precedence, Some(Precedence(None, None)))
          && matches!(top_operator_precedence, Some(Precedence(None, None)));

        if is_apply {
          operand = Some(Operator::Token(Token::Apply));
          skip = false;
        }

        match precedence {
          precedence @ Some(_) if top_operator_precedence <= precedence => {
            if skip {
              operands.next();
            }
            break FrameOperator {
              operator:   operand.unwrap(),
              precedence: precedence.unwrap(),
            };
          },
          _ => {
            let res = top;

            top = match stack.pop() {
              Some(it) => it,
              None => {
                return (
                  ExpressionParsingInput {
                    operands,
                    context,
                    errors,
                  },
                  res.lhs,
                )
              },
            };

            top.lhs = Some(match res.operator.unwrap().operator {
              Operator::Operand { operands, op } => Expression::from_options(
                match (&op[..], &operands[..]) {
                  ([Token::LParenthesis, Token::RParenthesis], [operands]) => {
                    let input = ExpressionParsingInput {
                      operands: operands.clone().into_iter().buffered(),
                      context:  context.clone(),
                      errors:   vec![],
                    };
                    let mut items = vec![];

                    let (mut input, item) = RecordItem::parse(input);
                    if let Some(item) = item {
                      items.push(item);

                      while let Some(Operator::Token(Token::Comma)) = input.operands.next() {
                        let (i, item) = RecordItem::parse(input);
                        if let Some(item) = item {
                          items.push(item);
                          input = i
                        } else {
                          break;
                        }
                      }
                    }

                    Expression::Record(items)
                  },
                  _ => {
                    let operands = operands
                      .into_iter()
                      .map(|operands| {
                        let input = ExpressionParsingInput {
                          context:  context.clone(),
                          errors:   vec![],
                          operands: operands.into_iter().buffered(),
                        };
                        Expression::parse(input)
                      })
                      .filter_map(|(mut i, o)| match o {
                        Some(o) => Some(o),
                        None => {
                          errors.append(&mut i.errors);
                          None
                        },
                      })
                      .collect_vec();

                    Expression::Mixfix { op, operands }
                  },
                },
                top.lhs,
                res.lhs,
              ),
              Operator::Token(Token::Apply) => Expression::Prefix {
                op:    Box::new(top.lhs.unwrap()),
                right: Box::new(res.lhs.unwrap()),
              },
              Operator::Token(token) => {
                Expression::from_options(Expression::Value(token), top.lhs, res.lhs)
              },
            });
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
}

impl<T: Iterator<Item = Operator>> Parseable<ExpressionParsingInput<T>> for RecordItem {
  fn parse(input: ExpressionParsingInput<T>) -> (ExpressionParsingInput<T>, Option<Self::O>) {
    let ExpressionParsingInput {
      mut operands,
      context,
      errors,
    } = input;

    if operands.peek().is_none() {
      return (
        ExpressionParsingInput {
          operands,
          context,
          errors,
        },
        None,
      );
    }

    let mut key = RecordKey::None;

    if let [Some(Operator::Token(Token::Identifier(src))), Some(Operator::Token(Token::Colon))] =
      &operands.peek_ext(2)[..]
    {
      key = RecordKey::Identifier(src.clone());
      operands.next_ext(2);
    } else if let [Some(Operator::Operand {
      operands: _operands,
      op,
    }), Some(Operator::Token(Token::Colon))] = &operands.peek_ext(2)[..]
    {
      match (&op[..], &_operands[..]) {
        ([Token::LBrace, Token::RBrace], [_operands]) => {
          let mut _operands = _operands.clone().into_iter().buffered();
          let (_, expr) = Expression::parse(ExpressionParsingInput {
            operands: _operands,
            context:  context.clone(),
            errors:   vec![],
          });
          if let Some(expr) = expr {
            key = RecordKey::Value(expr);
          } else {
            return (
              ExpressionParsingInput {
                operands,
                context,
                errors,
              },
              None,
            );
          }
        },
        _ => (),
      }
      operands.next_ext(2);
    }

    let (input, expr) = Expression::parse(ExpressionParsingInput {
      operands,
      context,
      errors,
    });

    if let Some(expr) = expr {
      (input, Some(RecordItem { key, value: expr }))
    } else {
      (input, None)
    }
  }
}
