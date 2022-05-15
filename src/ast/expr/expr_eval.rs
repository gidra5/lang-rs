use crate::{
  ast::{Evaluatable, RuntimeError},
  common::{value, LoggerTrait, Value},
  enviroment::Enviroment,
  map,
  scoped,
  token::Token,
  token_pat,
};
use itertools::Itertools;
use std::{collections::HashMap, fmt::Result, slice::SliceIndex};

use super::expr_struct::*;

pub fn match_value<L: LoggerTrait, F: FnMut(String, Value, &mut Enviroment)>(
  val: Value,
  pat: Expression,
  callback: &mut F,
  env: &mut Enviroment,
  logger: &mut L,
) -> std::result::Result<bool, RuntimeError> {
  Ok(match pat {
    // expr_pat!(value token: Equal, left: ident pat, right)
    Expression::Infix {
      left: pat,
      op: box Expression::Value(token_pat!(token: Equal)),
      right,
    } => {
      match_value(val, *pat.clone(), callback, env, logger)?
        && match_value((*right).evaluate(env, logger)?, *pat, callback, env, logger)?
    },
    Expression::Value(token_pat!(token: Placeholder)) => true,
    Expression::Value(token_pat!(token: Identifier, src: ident)) => {
      callback(ident, val, env);
      true
    },
    Expression::Value(token) => val == token.value(),

    Expression::Record(pat_rec) => {
      let mut iter = pat_rec.iter().peekable();
      match val {
        Value::Tuple(values) => {
          let mut val_iter = values.iter().peekable();

          loop {
            if let Some(RecordItem { key, value }) = iter.next() {
              if let Expression::Prefix {
                op: box Expression::Value(token_pat!(token: Spread)),
                right,
              } = value
              {
                break loop {
                  if let Some(RecordItem { key, value }) = iter.next_back() {
                    match (key, val_iter.next_back()) {
                      (RecordKey::None, Some(val)) => {
                        if !match_value(val.clone(), value.clone(), callback, env, logger)? {
                          break false;
                        }
                      },
                      _ => break false,
                    };
                  } else {
                    break match_value(
                      Value::Tuple(val_iter.cloned().collect_vec()),
                      *right.clone(),
                      callback,
                      env,
                      logger,
                    )?;
                  }
                };
              } else {
                if let (RecordKey::None, Some(val)) = (key, val_iter.next()) {
                  if !match_value(val.clone(), value.clone(), callback, env, logger)? {
                    break false;
                  }
                } else {
                  break false;
                };
              }
            } else {
              break true;
            }
          }
        },
        Value::Record(values) => {
          let mut val_copy = values.clone();

          loop {
            if let Some(RecordItem { key, value }) = iter.next() {
              if let Expression::Prefix {
                op: box Expression::Value(token_pat!(token: Spread)),
                right,
              } = value
              {
                break loop {
                  if let Some(RecordItem { key, value }) = iter.next_back() {
                    match key {
                      RecordKey::None => {
                        if let Expression::Value(token_pat!(token: Identifier, src: field)) = value
                        {
                          if let Some(v) = val_copy.remove(field) {
                            if !match_value(v, value.clone(), callback, env, logger)? {
                              break false;
                            }
                          } else {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      RecordKey::Identifier(field) => {
                        if let Some(v) = val_copy.remove(field) {
                          if !match_value(v, value.clone(), callback, env, logger)? {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      RecordKey::Value(expr) => {
                        let field = expr.evaluate(env, logger)?;
                        if let Value::String(ref field) = field {
                          if let Some(v) = val_copy.remove(field) {
                            if !match_value(v, value.clone(), callback, env, logger)? {
                              break false;
                            }
                          } else {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      _ => break false,
                    };
                  } else {
                    break match_value(
                      Value::Record(val_copy),
                      *right.clone(),
                      callback,
                      env,
                      logger,
                    )?;
                  }
                };
              } else {
                match key {
                  RecordKey::None => {
                    if let Expression::Value(token_pat!(token: Identifier, src: field)) = value {
                      if let Some(v) = val_copy.remove(field) {
                        if !match_value(v, value.clone(), callback, env, logger)? {
                          break false;
                        }
                      } else {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  RecordKey::Identifier(field) => {
                    if let Some(v) = val_copy.remove(field) {
                      if !match_value(v, value.clone(), callback, env, logger)? {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  RecordKey::Value(expr) => {
                    let field = expr.evaluate(env, logger)?;
                    if let Value::String(ref field) = field {
                      if let Some(v) = val_copy.remove(field) {
                        if !match_value(v, value.clone(), callback, env, logger)? {
                          break false;
                        }
                      } else {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  _ => break false,
                };
              }
            } else {
              break true;
            }
          }
        },
        Value::Map(values) => {
          let mut val_copy = values.clone();

          loop {
            if let Some(RecordItem { key, value }) = iter.next() {
              if let Expression::Prefix {
                op: box Expression::Value(token_pat!(token: Spread)),
                right,
              } = value
              {
                break loop {
                  if let Some(RecordItem { key, value }) = iter.next_back() {
                    match key {
                      RecordKey::None => {
                        if let Expression::Value(token_pat!(token: Identifier, src: field)) = value
                        {
                          if let Some(v) = val_copy.remove(&Value::String(field.clone())) {
                            if !match_value(v, value.clone(), callback, env, logger)? {
                              break false;
                            }
                          } else {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      RecordKey::Identifier(field) => {
                        if let Some(v) = val_copy.remove(&Value::String(field.clone())) {
                          if !match_value(v, value.clone(), callback, env, logger)? {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      RecordKey::Value(expr) => {
                        let field = expr.evaluate(env, logger)?;
                        if let Some(v) = val_copy.remove(&field) {
                          if !match_value(v, value.clone(), callback, env, logger)? {
                            break false;
                          }
                        } else {
                          break false;
                        }
                      },
                      _ => break false,
                    };
                  } else {
                    break match_value(
                      Value::Map(val_copy),
                      *right.clone(),
                      callback,
                      env,
                      logger,
                    )?;
                  }
                };
              } else {
                match key {
                  RecordKey::None => {
                    if let Expression::Value(token_pat!(token: Identifier, src: field)) = value {
                      if let Some(v) = val_copy.remove(&Value::String(field.clone())) {
                        if !match_value(v, value.clone(), callback, env, logger)? {
                          break false;
                        }
                      } else {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  RecordKey::Identifier(field) => {
                    if let Some(v) = val_copy.remove(&Value::String(field.clone())) {
                      if !match_value(v, value.clone(), callback, env, logger)? {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  RecordKey::Value(expr) => {
                    let field = expr.evaluate(env, logger)?;
                    if let Some(v) = val_copy.remove(&field) {
                      if !match_value(v, value.clone(), callback, env, logger)? {
                        break false;
                      }
                    } else {
                      break false;
                    }
                  },
                  _ => break false,
                }
              }
            } else {
              break true;
            }
          }
        },
        _ => false,
      }
    },
    _ => false,
  })
}

fn fact(n: f64) -> f64 {
  if n <= 1. {
    1.
  } else {
    n * fact(n - 1.)
  }
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(
    &self,
    env: &mut Enviroment,
    logger: &mut L,
  ) -> std::result::Result<value::Value, RuntimeError> {
    Ok(match self {
      Expression::For(for_pat, iter, body) => {
        let mut iterator = iter.evaluate(env, logger)?;
        let mut accumulator = Value::None;

        loop {
          if let Value::Function(ref fn_pat, ref mut fn_env, ref expr) = iterator {
            let next = scoped!(fn_env, {
              match_value(
                accumulator,
                fn_pat.clone(),
                &mut |ident, val, fn_env| fn_env.define(ident, val),
                fn_env,
                logger,
              );

              expr.evaluate(fn_env, logger)?
            });

            let val = if let Value::Tuple(vec) = next {
              iterator = vec[1].clone();
              vec[0].clone()
            } else {
              iterator = Value::None;
              next
            };

            accumulator = scoped!(env, {
              match_value(
                val.clone(),
                (*for_pat.clone()),
                &mut |ident, val, env| env.define(ident, val),
                env,
                logger,
              )?;

              body.evaluate(env, logger)?
            })
          } else {
            break accumulator;
          }
        }
      },
      Expression::If(condition, true_branch, false_branch) => {
        if let Value::Boolean(true) = (*condition).evaluate(env, logger)? {
          (*true_branch).evaluate(env, logger)?
        } else if let Some(false_branch) = false_branch {
          (*false_branch).evaluate(env, logger)?
        } else {
          Value::None
        }
      },
      Expression::Value(token_pat!(token: Identifier, src: id)) => env.get(&id).unwrap_or_default(),
      Expression::Infix {
        left: box Expression::Value(token_pat!(token: Identifier, src: id)),
        op: box Expression::Value(token_pat!(token: Hash)),
        right,
      } => match (*right).evaluate(env, logger)? {
        Value::Number(n) => env.get_from(n as usize, &id).unwrap_or_default(),
        _ => env.get(&id).unwrap_or_default(),
      },
      Expression::Value(op) => op.value(),
      Expression::Postfix {
        left,
        op: box Expression::Value(token_pat!(token)),
      } => match (token, (*left).evaluate(env, logger)?) {
        (Token::Bang, Value::Number(num)) => Value::Number(fact(num)),
        _ => Value::None,
      },
      Expression::Prefix {
        op: box left,
        right,
      } => match left {
        Expression::Value(token_pat!(token: Identifier, src)) if src == "parse" => {
          logger.write(format!("{}", right));
          (*right).evaluate(env, logger)?
        },
        Expression::Value(token_pat!(token: Identifier, src)) if src == "print" => {
          let value = (*right).evaluate(env, logger)?;
          logger.write(format!("{}", value));
          value
        },
        Expression::Value(token_pat!(token: Identifier, src)) if src == "let" => {
          if let box Expression::Infix {
            left: pat,
            op: box Expression::Value(token_pat!(token: Equal)),
            right,
          } = right.clone()
          {
            let right = (*right).evaluate(env, logger)?;

            match_value(
              right.clone(),
              *pat.clone(),
              &mut |ident, val, env| {
                env.define(ident, val);
              },
              env,
              logger,
            );

            right
          } else {
            match_value(
              Value::None,
              *right.clone(),
              &mut |ident, val, env| {
                env.define(ident, val);
              },
              env,
              logger,
            )?;
            Value::None
          }
        },
        left @ Expression::Value(token_pat!(token: @token Sub | Bang | Dec | Inc)) => {
          match (token, (*right).evaluate(env, logger)?) {
            (Token::Sub, Value::Number(num)) => Value::Number(-num),
            (Token::Bang, Value::Boolean(val)) => Value::Boolean(!val),
            (Token::Dec, Value::Number(val)) => Value::Number(val - 1.),
            (Token::Inc, Value::Number(val)) => Value::Number(val + 1.),
            _ => Value::None,
          }
        },
        left => {
          let mut left = (*left).clone().evaluate(env, logger)?;

          if let Value::Function(pat, ref mut fn_env, expr) = left.clone() {
            scoped!(fn_env, {
              fn_env.define("self".to_string(), left);

              match_value(
                (*right).evaluate(env, logger)?,
                pat,
                &mut |ident, val, fn_env| fn_env.define(ident, val),
                fn_env,
                logger,
              );

              expr.evaluate(fn_env, logger)?
            })
          } else {
            Value::None
          }
        },
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Arrow)),
        right,
      } => Value::Function(*left.clone(), Box::new(env.clone()), *right.clone()),
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Is)),
        right,
      } => {
        let left = (*left).evaluate(env, logger)?;

        Value::Boolean(match_value(
          left,
          (*right.clone()),
          &mut |_, _, _| (),
          env,
          logger,
        )?)
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Apply)),
        right,
      } => {
        let mut left = (*left).clone().evaluate(env, logger)?;

        if let Value::Function(pat, ref mut fn_env, expr) = left.clone() {
          scoped!(fn_env, {
            fn_env.define("self".to_string(), left);

            match_value(
              (*right).evaluate(env, logger)?,
              pat,
              &mut |ident, val, fn_env| fn_env.define(ident, val),
              fn_env,
              logger,
            );

            expr.evaluate(fn_env, logger)?
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
        let right = (*right).evaluate(env, logger)?;

        match_value(
          right.clone(),
          *pat.clone(),
          &mut |ident, val, env| {
            env.set(ident, val);
          },
          env,
          logger,
        )?;

        right
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token: Period)),
        right: box Expression::Value(token_pat!(token: Identifier | Number, src)),
      } => match (*left).evaluate(env, logger)? {
        Value::Record(left) => left.get(src).cloned().unwrap_or_default(),
        _ => Value::None,
      },
      Expression::Infix {
        left,
        op: box Expression::Value(token_pat!(token, src)),
        right,
      } => {
        match (
          (*left).evaluate(env, logger)?,
          token,
          (*right).evaluate(env, logger)?,
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
          (Value::Tuple(left), Token::LBrace, Value::Number(right))
            if right == (right as usize) as f64 =>
          {
            left.get(right as usize).cloned().unwrap_or_default()
          },
          (Value::Record(left), Token::LBrace, Value::Number(right))
            if right == (right as usize) as f64 =>
          {
            left
              .get(&format!("{}", right as usize))
              .cloned()
              .unwrap_or_default()
          },
          (Value::Record(left), Token::LBrace, Value::String(ref right)) => {
            left.get(right).cloned().unwrap_or_default()
          },
          (Value::Map(left), Token::LBrace, ref right) => {
            left.get(right).cloned().unwrap_or_default()
          },
          (left, Token::Identifier, right) => match (left, src.as_str(), right) {
            (Value::Number(left), "mod", Value::Number(right)) => Value::Number(left % right),
            (Value::Boolean(left), "and", Value::Boolean(right)) => Value::Boolean(left && right),
            (Value::Boolean(left), "or", Value::Boolean(right)) => Value::Boolean(left || right),
            _ => Value::None,
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
                stmt.evaluate(env, logger)?;
              } else {
                break stmt.evaluate(env, logger)?;
              }
            } else {
              break Value::None;
            }
          }
        })
      },
      Expression::Record(op) => {
        let mut tuple_values = vec![];
        let mut iter = op.iter().peekable();

        loop {
          if let Some(RecordItem { key, value }) = iter.peek() {
            if let Expression::Prefix {
              op: box Expression::Value(token_pat!(token: Spread)),
              right,
            } = value
            {
              if let Value::Tuple(mut values) = right.evaluate(env, logger)? {
                tuple_values.append(&mut values);
              } else {
                break;
              }
            } else {
              let tuple_val = value.evaluate(env, logger)?;
              match key {
                RecordKey::None => tuple_values.push(tuple_val),
                _ => break,
              };
            }
            iter.next();
          } else {
            return Ok(Value::Tuple(tuple_values));
          }
        }

        let mut record_values = tuple_values
          .into_iter()
          .enumerate()
          .map(|(i, x)| (format!("{}", i), x))
          .collect::<HashMap<String, Value>>();

        loop {
          if let Some(RecordItem { key, value }) = iter.peek() {
            if let Expression::Prefix {
              op: box Expression::Value(token_pat!(token: Spread)),
              right,
            } = value
            {
              let spread_val = right.evaluate(env, logger)?;
              if let Value::Tuple(values) = spread_val {
                for x in values {
                  record_values.insert(format!("{}", record_values.len()), x);
                }
              } else if let Value::Record(mut values) = spread_val {
                for (x, y) in values {
                  record_values.insert(x, y);
                }
              } else {
                break;
              }
            } else {
              let record_val = value.evaluate(env, logger)?;
              match key {
                RecordKey::None => {
                  record_values.insert(format!("{}", record_values.len()), record_val)
                },
                RecordKey::Identifier(id) => record_values.insert(id.clone(), record_val),
                _ => break,
              };
            }
            iter.next();
          } else {
            return Ok(Value::Record(record_values));
          }
        }

        let mut map_values = record_values
          .into_iter()
          .map(|(i, x)| (Value::String(i), x))
          .collect::<HashMap<Value, Value>>();

        loop {
          if let Some(RecordItem { key, value }) = iter.next() {
            if let Expression::Prefix {
              op: box Expression::Value(token_pat!(token: Spread)),
              right,
            } = value
            {
              let spread_val = right.evaluate(env, logger)?;

              if let Value::Tuple(values) = spread_val {
                for x in values {
                  map_values.insert(Value::String(format!("{}", map_values.len())), x);
                }
              } else if let Value::Record(mut values) = spread_val {
                for (x, y) in values {
                  map_values.insert(Value::String(x), y);
                }
              } else if let Value::Map(mut values) = spread_val {
                for (x, y) in values {
                  map_values.insert(x, y);
                }
              } else {
                break;
              }
            } else {
              let map_val = value.evaluate(env, logger)?;
              match key {
                RecordKey::None => {
                  map_values.insert(Value::String(format!("{}", map_values.len())), map_val)
                },
                RecordKey::Identifier(id) => map_values.insert(Value::String(id.clone()), map_val),
                RecordKey::Value(expr) => map_values.insert(expr.evaluate(env, logger)?, map_val),
              };
            }
          } else {
            return Ok(Value::Map(map_values));
          }
        }

        Value::None
      },
      _ => Value::None,
    })
  }
}
