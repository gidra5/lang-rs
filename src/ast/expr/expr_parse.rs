use std::cmp::Ordering;

use crate::{
  ast::{Parseable, Statement},
  check_token,
  check_token_end,
  common::{ReversableIterator, Span},
  match_token,
  punct_or_newline,
  skip,
  token::{Token, TokenExt, TokenStream},
  token_pat,
};

use super::expr_struct::{Expression, Op, RecordItem, RecordKey};

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

#[derive(PartialEq, Default, Debug, Clone)]
pub struct Operator {
  fixity: Fixity,
  token:  TokenExt,
}


#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

fn parse_for(token_stream: &mut TokenStream) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), For) {
    Err("Expected for expression".to_string())
  } else {
    token_stream.next();

    let pat = Expression::parse(token_stream)?;

    if !check_token!(token_stream.next(), In) {
      return Err("Expected 'in' after identifier".to_string());
    }

    let iterator = Expression::parse(token_stream)?;

    if !punct_or_newline!(token_stream.next(), Colon) {
      return Err("Missing colon after expression in for statement".to_string());
    }

    let body = Expression::parse(token_stream)?;

    return Ok(Expression {
      left:  None,
      right: None,
      op:    Op::For(Box::new(pat), Box::new(iterator), Box::new(body)),
    });
  }
}

fn parse_if(token_stream: &mut TokenStream) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), If) {
    Err("Expected if expression".to_string())
  } else {
    token_stream.next();
    let expr = Expression::parse(token_stream)?;

    if !punct_or_newline!(token_stream.next(), Colon) {
      return Err("Missing colon after condition expression in an if expression".to_string());
    }

    let true_block =
      Expression::parse(token_stream).map_err(|err| format!("Can't parse true branch: {}", err))?;

    if true_block == Expression::default() {
      return Err("Empty true branch in if expression".to_string());
    }
    let y = token_stream.peek_ext(2);

    let false_block = if check_token!(token_stream.peek(), Else)
      || check_token!(y[0], NewLine) && check_token!(y[1], Else)
    {
      skip!(token_stream, NewLine);
      token_stream.next();
      skip!(token_stream, NewLine);

      let false_block = Expression::parse(token_stream)
        .map_err(|err| format!("Can't parse false branch: {}", err))?;

      if false_block == Expression::default() {
        return Err("Empty false branch in if expression".to_string());
      }

      Some(Box::new(false_block))
    } else {
      None
    };

    return Ok(Expression {
      left:  None,
      right: None,
      op:    Op::If(Box::new(expr), Box::new(true_block), false_block),
    });
  }
}

fn parse_braces(token_stream: &mut TokenStream) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), LBrace) {
    Err("Expected opening brace".to_string())
  } else {
    token_stream.next();

    let expr = Expression::parse(token_stream)?;

    if !check_token!(token_stream.next(), RBrace) {
      Err("Expected closing brace".to_string())
    } else {
      Ok(expr)
    }
  }
}

fn parse_block(token_stream: &mut TokenStream) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), LBracket) {
    Err("Expected opening bracket".to_string())
  } else {
    token_stream.next();

    let x = {
      let mut res = vec![];

      loop {
        if !check_token!(token_stream.peek(), RBracket) && !check_token_end!(token_stream) {
          match Statement::parse(token_stream) {
            Ok(Statement::Expression(expr)) if expr == Expression::default() => (),
            Ok(stmt) => res.push(stmt),
            Err(msg) => return Err(msg),
          };
        } else {
          break res;
        }
      }
    };

    if !check_token!(token_stream.next(), RBracket) {
      Err("Expected closing bracket".to_string())
    } else {
      Ok(Expression {
        left:  None,
        right: None,
        op:    Op::Block(x),
      })
    }
  }
}

fn parse_parens(token_stream: &mut TokenStream) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), LParenthesis) {
    Err("Expected opening parenthesis".to_string())
  } else {
    token_stream.next();
    let mut x = vec![];

    if check_token!(token_stream.peek(), RParenthesis) {
      token_stream.next();

      return Ok(Expression {
        left:  None,
        right: None,
        op:    Op::Record(x),
      });
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
        key = RecordKey::Value(parse_expr(token_stream, true)?);
        let y = token_stream.peek_ext(2);

        if !(check_token!(y[0], RBrace) && check_token!(y[1], Colon)) {
          return Err("Expected closing brace and colon".to_string());
        } else {
          token_stream.next_ext(2);
        }
      }

      let expr = parse_expr(token_stream, true)?;

      skip!(token_stream, Comma);

      if check_token_end!(token_stream) {
        return Err("Unexpected end of input".to_string());
      }

      x.push(RecordItem { key, value: expr });
    }
    token_stream.next();

    Ok(Expression {
      left:  None,
      right: None,
      op:    Op::Record(x),
    })
  }
}

fn parse_expr(token_stream: &mut TokenStream, in_parens: bool) -> Result<Expression, String> {
  let mut top = Frame {
    lhs:      None,
    operator: None,
  };
  let mut stack = Vec::new();

  loop {
    if check_token!(token_stream.peek(), NewLine) && in_parens {
      token_stream.next();
      continue;
    }

    let mut token = token_stream.peek();

    match token {
      match_token!(For) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  token_pat!(token: Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_for(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_for(token_stream)?);
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(If) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  token_pat!(token: Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_if(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_if(token_stream)?);
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(RParenthesis) if !in_parens => {
        return Err("Unexpected closing parenthesis".to_string());
      },
      match_token!(LParenthesis) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  token_pat!(token: Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_parens(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_parens(token_stream)?);
        }

        if let Some(
          op @ Operator {
            fixity: Fixity::None,
            ..
          },
        ) = top.operator.clone()
        {
          let res = top;

          top = match stack.pop() {
            Some(it) => it,
            None => return Ok(res.lhs.unwrap_or_default()),
          };

          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(Expression {
              op:    Op::Value(op.token),
              right: None,
              left:  None,
            })),
            right: res.lhs.map(Box::new),
          });
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(LBracket) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  token_pat!(token: Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_block(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_block(token_stream)?);
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(LBrace) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  token_pat!(token: Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        top.lhs = Some(Expression {
          op:    Op::Value(token.unwrap()),
          right: Some(Box::new(parse_braces(token_stream)?)),
          left:  Some(Box::new(match stack.pop() {
            None => top.lhs.ok_or("Unexpected indexing position")?,
            Some(Frame { lhs, operator }) => {
              let op = Op::Value(top.operator.ok_or("Unexpected indexing position")?.token);
              top.operator = operator;


              Expression {
                op,
                left: lhs.map(Box::new),
                right: top.lhs.map(Box::new),
              }
            },
          })),
        });
        continue;
      },
      _ => (),
    };
    // println!(
    //   "1 ===\n\n{:?}\n\n{:?}\n\n{:?}\n\n{:?}",
    //   stack, top, token, in_parens
    // );

    let operator = loop {
      let mut operator = token
        .clone()
        .map(|token| Operator::new(token, top.lhs.is_none()))
        .flatten();

      match operator {
        Some(op) if top.operator <= Some(op.clone()) => break op,
        x => {
          // println!("2 ===\n\n{:?}\n\n{:?}\n\n{:?}", stack, top, x);
          if matches!(
            x,
            Some(Operator {
              fixity: Fixity::None,
              token:  _,
            })
          ) && matches!(
            token.clone().map(|token| Operator::new(token, false)),
            Some(None)
          ) {
            token_stream.backtrack(1);
            token = token.map(|t| {
              TokenExt {
                token: Token::Apply,
                ..t
              }
            });
          } else {
            let res = top;

            top = match stack.pop() {
              Some(it) => it,
              None => return Ok(res.lhs.unwrap_or_default()),
            };

            if !matches!(
              res.operator,
              Some(Operator {
                fixity: Fixity::None,
                token:  token_pat!(token: Skip),
              })
            ) {
              top.lhs = Some(Expression {
                op:    Op::Value(res.operator.unwrap().token),
                left:  top.lhs.map(Box::new),
                right: res.lhs.map(Box::new),
              });
            }
          }
        },
      };
    };
    token_stream.next();

    stack.push(top);
    top = Frame {
      lhs:      None,
      operator: Some(operator),
    };
  }
}

impl Parseable for Expression {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> { parse_expr(stream, false) }
}

impl Operator {
  pub fn new(token: TokenExt, prefix: bool) -> Option<Self> {
    let op = Operator {
      token,
      fixity: if prefix {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    };

    if op.exists() {
      return Some(op);
    }

    let op = Operator {
      fixity: if prefix {
        Fixity::None
      } else {
        Fixity::Postfix
      },
      ..op
    };

    if op.exists() {
      return Some(op);
    }

    None
  }

  fn exists(&self) -> bool { self.bp().is_some() }

  fn bp(&self) -> Option<(u8, u8)> {
    Some(match self {
      Operator {
        token: token_pat!(token: Identifier | String | Placeholder | Char | Number | Boolean),
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        token: token_pat!(token, src),
        fixity: Fixity::Prefix,
      } => {
        (99, match token {
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
        })
      },
      Operator {
        token: token_pat!(token, src),
        fixity: Fixity::Postfix,
      } => {
        (
          match token {
            Token::Bang => 15,
            _ => return None,
          },
          100,
        )
      },
      Operator {
        token: token_pat!(token, src),
        fixity: Fixity::Infix,
      } => {
        match token {
          Token::LBrace => (26, 27),
          Token::Period => (24, 23),
          Token::Equal => (97, 1),
          Token::Mod => (28, 29),
          Token::Add | Token::Sub => (5, 6),
          Token::Mult | Token::Div => (7, 8),
          Token::EqualEqual => (20, 19),
          Token::LAngleBracket => (20, 19),
          Token::RAngleBracket => (20, 19),
          Token::LessEqual => (20, 19),
          Token::GreaterEqual => (20, 19),
          Token::Arrow => (37, 0),
          Token::Apply => (34, 35),
          Token::Is => (32, 33),
          Token::Hash => (97, 97),

          Token::Identifier if src == "mod" => (22, 21),
          Token::Identifier if src == "and" => (24, 23),
          Token::Identifier if src == "or" => (25, 26),
          _ => return None,
        }
      },
      _ => return None,
    })
  }
}

impl PartialOrd for Operator {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    let (_, r_bp1) = self.bp()?;
    let (l_bp2, _) = other.bp()?;

    Some(match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
      (false, false) => Ordering::Equal,
      (true, false) => Ordering::Less,
      (false, true) => Ordering::Greater,
      _ => return None,
    })
  }
}
