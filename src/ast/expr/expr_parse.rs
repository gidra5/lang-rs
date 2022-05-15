use itertools::Itertools;

use crate::{
  ast::ParsingInput,
  common::{Buf, Buffered},
  errors::ParsingError,
  parseable::{Parseable, ParseableIterator, ParsingContext},
  token::Token,
};

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
      errors,
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
              Operator::Operand { operands, op } => {
                let operands = operands
                  .into_iter()
                  .map(|operands| {
                    let input = ExpressionParsingInput {
                      context:  context.clone(),
                      errors:   errors.clone(),
                      operands: operands.into_iter().buffered(),
                    };
                    Expression::parse(input).1.unwrap()
                  })
                  .collect_vec();

                Expression::from_options(Expression::Mixfix { op, operands }, top.lhs, res.lhs)
              },
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

impl<T: Iterator<Item = Token>> Parseable<ParsingInput<T>> for Expression {
  fn parse(input: ParsingInput<T>) -> (ParsingInput<T>, Option<Self::O>) {
    let context = input.context.clone();
    let (
      ExpressionParsingInput {
        operands,
        context,
        errors,
      },
      o,
    ) = <Expression as Parseable<ExpressionParsingInput<_>>>::parse(ExpressionParsingInput {
      operands: <ParsingInput<T> as ParseableIterator<Operator>>::parsed(input).buffered(),
      context,
      errors: vec![],
    });

    (
      ParsingInput {
        tokens: operands.iterator.source.tokens,
        context,
        errors,
      },
      o,
    )
  }
}
