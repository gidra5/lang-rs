use crate::{
  ast::Evaluatable,
  common::{value, LoggerTrait, Value},
  enviroment::Enviroment,
  scoped,
  token::Token,
  token_pat,
};

use super::expr_struct::*;

pub fn match_value<L: LoggerTrait>(
  bind: usize,
  val: Value,
  pat: Expression,
  env: &mut Enviroment,
  logger: &mut L,
) -> bool {
  match pat {
    // expr_pat!(value token: Equal, left: ident pat, right)
    Expression::Infix {
      left: pat,
      op: box Expression::Value(token_pat!(token: Equal)),
      right,
    } => {
      let res = match_value(bind, val, *pat.clone(), env, logger);

      if bind == 1 && !res {
        match_value(bind, (*right).evaluate(env, logger), *pat, env, logger)
      } else {
        res
      }
    },
    Expression::Value(token_pat!(token: Placeholder)) => true,
    Expression::Value(token_pat!(token: Identifier, src: ident)) => {
      if bind == 1 {
        env.define(ident, val);
      } else if bind == 2 {
        env.set(ident, val);
      }
      true
    },
    Expression::Value(token) => val == token.value(),

    Expression::Record(pat_rec) => {
      match val {
        Value::Record(val_rec) => {
          pat_rec.len() == val_rec.len()
            && pat_rec.into_iter().zip(val_rec).all(
              |(
                RecordItem {
                  key: pat_key,
                  value: pat,
                },
                value::RecordItem {
                  key: val_key,
                  value: val,
                },
              )| {
                (match (pat.clone(), pat_key, val_key) {
                  (_, RecordKey::None, None) => true,
                  (
                    Expression::Value(token_pat!(token: Identifier, src: x)),
                    RecordKey::None,
                    Some(Value::String(y)),
                  ) => x == y,
                  (_, RecordKey::Identifier(x), Some(Value::String(y))) => *x == y,
                  (_, RecordKey::Value(x), Some(y)) => x.evaluate(env, logger) == y,
                  _ => false,
                }) && match_value(bind, val, pat, env, logger)
              },
            )
        },
        Value::Unit => pat_rec.len() == 0,
        _ => false,
      }
    },
    _ => false,
  }
}

fn fact(n: f64) -> f64 {
  if n <= 1. {
    1.
  } else {
    n * fact(n - 1.)
  }
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    match self {
      Expression::For(for_pat, iter, body) => {
        let mut iterator = iter.evaluate(env, logger);
        let mut accumulator = Value::Unit;

        return loop {
          if let Value::Function(ref fn_pat, ref mut fn_env, ref expr) = iterator {
            let next = scoped!(fn_env, {
              match_value(1, accumulator, (*fn_pat.clone()), fn_env, logger);

              expr.evaluate(fn_env, logger)
            });

            let val = if let Value::Record(vec) = next {
              let value::RecordItem { value: val, .. } = &vec[0];
              let value::RecordItem { value: iter, .. } = &vec[1];
              iterator = iter.clone();
              val.clone()
            } else {
              iterator = Value::None;
              next
            };

            accumulator = scoped!(env, {
              match_value(1, val.clone(), (*for_pat.clone()), env, logger);

              body.evaluate(env, logger)
            })
          } else {
            break accumulator;
          }
        };

        // return loop {
        //   let mut stmts = body.iter().cloned().peekable();

        //   if let Value::Function(pat, ref mut fn_env, expr) = iterator {
        //     let next = scoped!(fn_env, {
        //       match_value(1, accumulator, (*pat), fn_env, logger);

        //       expr.evaluate(fn_env, logger)
        //     });

        //     if let Value::Record(vec) = next {
        //       let RecordItem { value: val, .. } = &vec[0];
        //       let RecordItem { value: iter, .. } = &vec[1];

        //       scoped!(env, {
        //         env.define(var.clone(), val.clone());

        //         iterator = iter.clone();

        //         accumulator = loop {
        //           if let Some(stmt) = stmts.next() {
        //             if let Some(_) = stmts.peek() {
        //               stmt.evaluate(env, logger);
        //             } else {
        //               break stmt.evaluate(env, logger);
        //             }
        //           }
        //         }
        //       })
        //     } else {
        //       env.define(var.clone(), next);
        //       break scoped!(env, {
        //         loop {
        //           if let Some(stmt) = stmts.next() {
        //             if let Some(_) = stmts.peek() {
        //               stmt.evaluate(env, logger);
        //             } else {
        //               break stmt.evaluate(env, logger);
        //             }
        //           }
        //         }
        //       });
        //     }
        //   } else {
        //     env.define(var.clone(), iterator);
        //     break scoped!(env, {
        //       loop {
        //         if let Some(stmt) = stmts.next() {
        //           if let Some(_) = stmts.peek() {
        //             stmt.evaluate(env, logger);
        //           } else {
        //             break stmt.evaluate(env, logger);
        //           }
        //         }
        //       }
        //     });
        //   }
        // };
      },
      Expression::If(condition, true_branch, false_branch) => {
        if let Value::Boolean(true) = (*condition).evaluate(env, logger) {
          (*true_branch).evaluate(env, logger)
        } else if let Some(false_branch) = false_branch {
          (*false_branch).evaluate(env, logger)
        } else {
          Value::None
        }
      },
      Expression::Value(token_pat!(token: Identifier, src: id)) => env.get(&id).unwrap_or_default(),
      Expression::Infix {
        left: box Expression::Value(token_pat!(token: Identifier, src: id)),
        op: box Expression::Value(token_pat!(token: Hash)),
        right,
      } => {
        match (*right).evaluate(env, logger) {
          Value::Number(n) => env.get_from(n as usize, &id).unwrap_or_default(),
          _ => env.get(&id).unwrap_or_default(),
        }
      },
      Expression::Value(op) => op.value(),
      Expression::Postfix {
        left,
        op: box Expression::Value(token_pat!(token)),
      } => {
        match (token, (*left).evaluate(env, logger)) {
          (Token::Bang, Value::Number(num)) => Value::Number(fact(num)),
          _ => Value::None,
        }
      },
      Expression::Prefix {
        op: box Expression::Value(token_pat!(token: Identifier, src)),
        right,
      } if src == "parse" => {
        logger.write(format!("{}", right));
        (*right).evaluate(env, logger)
      },
      Expression::Prefix {
        op: box left,
        right,
      } => {
        match left {
          Expression::Value(token_pat!(token: Identifier, src)) if src == "print" => {
            let value = (*right).evaluate(env, logger);
            logger.write(format!("{}", value));
            value
          },
          left @ Expression::Value(token_pat!(token: @token Sub | Bang | Dec | Inc)) => {
            match (token, (*right).evaluate(env, logger)) {
              (Token::Sub, Value::Number(num)) => Value::Number(-num),
              (Token::Bang, Value::Boolean(val)) => Value::Boolean(!val),
              (Token::Dec, Value::Number(val)) => Value::Number(val - 1.),
              (Token::Inc, Value::Number(val)) => Value::Number(val + 1.),
              _ => Value::None,
            }
          },
          left => {
            let mut left = (*left).clone().evaluate(env, logger);

            if let Value::Function(pat, ref mut fn_env, expr) = left.clone() {
              scoped!(fn_env, {
                fn_env.define("self".to_string(), left);

                match_value(1, (*right).evaluate(env, logger), (*pat), fn_env, logger);

                expr.evaluate(fn_env, logger)
              })
            } else {
              Value::None
            }
          },
        }
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Arrow)),
        right,
      } => Value::Function(left.clone(), Box::new(env.clone()), right.clone()),
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Is)),
        right,
      } => {
        let left = (*left).evaluate(env, logger);

        Value::Boolean(match_value(0, left, (*right.clone()), env, logger))
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Apply)),
        right,
      } => {
        let mut left = (*left).clone().evaluate(env, logger);

        if let Value::Function(pat, ref mut fn_env, expr) = left.clone() {
          scoped!(fn_env, {
            fn_env.define("self".to_string(), left);

            match_value(1, (*right).evaluate(env, logger), (*pat), fn_env, logger);

            expr.evaluate(fn_env, logger)
          })
        } else {
          Value::None
        }
      },
      Expression::Infix {
        left: pat,
        op: box Expression::Value(token_pat!(token: Equal)),
        right,
      } => {
        let right = (*right).evaluate(env, logger);
        match_value(2, right.clone(), *pat.clone(), env, logger);
        right
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Period)),
        right: box Expression::Value(token_pat!(token: Identifier, src)),
      } => {
        match (*left).evaluate(env, logger) {
          Value::Record(left) => {
            left
              .iter()
              .find_map(|value::RecordItem { key, value }| {
                match key {
                  Some(Value::String(name)) if name.clone() == src.clone() => Some(value.clone()),
                  _ => None,
                }
              })
              .unwrap_or_default()
          },
          _ => Value::None,
        }
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token, src)),
        right,
      } => {
        match (
          (*left).evaluate(env, logger),
          token,
          (*right).evaluate(env, logger),
        ) {
          (Value::Number(left), Token::Sub, Value::Number(right)) => Value::Number(left - right),
          (Value::Number(left), Token::Add, Value::Number(right)) => Value::Number(left + right),
          (Value::Number(left), Token::Mult, Value::Number(right)) => Value::Number(left * right),
          (Value::Number(left), Token::Div, Value::Number(right)) => Value::Number(left / right),
          (Value::Number(left), Token::Pow, Value::Number(right)) => {
            Value::Number(left.powf(right))
          },
          (Value::Number(left), Token::EqualEqual, Value::Number(right)) => {
            Value::Boolean((left - right).abs() < f64::EPSILON)
          },
          (Value::Number(left), Token::LessEqual, Value::Number(right)) => {
            Value::Boolean(left <= right)
          },
          (Value::Number(left), Token::GreaterEqual, Value::Number(right)) => {
            Value::Boolean(left >= right)
          },
          (Value::Number(left), Token::LAngleBracket, Value::Number(right)) => {
            Value::Boolean(left < right)
          },
          (Value::Number(left), Token::RAngleBracket, Value::Number(right)) => {
            Value::Boolean(left > right)
          },
          (Value::Record(left), Token::LBrace, Value::Number(right))
            if right == (right as usize) as f64 && left[0].key == None =>
          {
            left[right as usize].value.clone()
          },
          (Value::Record(left), Token::LBrace, right) => {
            left
              .iter()
              .find_map(|value::RecordItem { key, value }| {
                match key {
                  Some(key) if key.clone() == right => Some(value.clone()),
                  _ => None,
                }
              })
              .unwrap_or_default()
          },
          (left, Token::Identifier, right) => {
            match (left, src.as_str(), right) {
              (Value::Number(left), "mod", Value::Number(right)) => Value::Number(left % right),
              (Value::Boolean(left), "and", Value::Boolean(right)) => Value::Boolean(left && right),
              (Value::Boolean(left), "or", Value::Boolean(right)) => Value::Boolean(left || right),
              _ => Value::None,
            }
          },
          _ => Value::None,
        }
      },
      Expression::Block(statements) => {
        scoped!(env, {
          let mut iter = statements.into_iter().peekable();

          loop {
            if let Some(stmt) = iter.next() {
              if let Some(_) = iter.peek() {
                stmt.evaluate(env, logger);
              } else {
                break stmt.evaluate(env, logger);
              }
            } else {
              break Value::None;
            }
          }
        })
      },
      Expression::Record(op) => {
        let mut x = op
          .into_iter()
          .map(|RecordItem { key, value }| {
            value::RecordItem {
              key:   match key {
                RecordKey::Identifier(name) => Some(Value::String(name.clone())),
                RecordKey::Value(expr) => Some(expr.evaluate(env, logger)),
                RecordKey::None => None,
              },
              value: value.evaluate(env, logger),
            }
          })
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].key != None) {
          Value::Record(x)
        } else if x.len() == 1 {
          x.pop().unwrap().value
        } else {
          Value::Unit
        }
      },
      _ => Value::None,
    }
  }
}
