use std::cmp::Ordering;

use crate::{namespace::Declaration, parseable::ParsingContext, token::Token};

#[derive(Clone, PartialEq, Default, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,

  #[default]
  None,
}

#[derive(PartialEq, Default, Debug, Clone)]
pub struct Precedence(pub Option<u8>, pub Option<u8>);

#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
  Operand {
    /// given operator with gaps (like `(_)` or `if_then_else_`, `_` where gaps)
    /// will generate sequences of tokens for each of the inner gaps, outer gaps
    /// are decided depending on precedence
    operands: Vec<Vec<Operator>>,
    /// operator, the separator tokens
    op:       Vec<Token>,
  },
  Token(Token),
}

impl PartialOrd for Precedence {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    // aka r_bp1 < l_bp2
    let (Precedence(_, r_bp1), Precedence(l_bp2, _)) = (self, other);
    Some(match (r_bp1, l_bp2) {
      (None, None) => Ordering::Greater,
      (Some(_), None) => Ordering::Less,
      (None, Some(_)) => Ordering::Less,
      (Some(r_bp1), Some(l_bp2)) => match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
        (false, false) => Ordering::Equal,
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        _ => return None,
      },
    })
  }
}

impl Operator {
  pub fn new(token: Token) -> Self { Self::Token(token) }

  pub fn get_precedence(&self, context: &mut ParsingContext, prefix: bool) -> Option<Precedence> {
    self
      .get_precedence_fixity(
        context,
        if prefix {
          Fixity::Prefix
        } else {
          Fixity::Infix
        },
      )
      .or_else(|| {
        self.get_precedence_fixity(
          context,
          if prefix {
            Fixity::None
          } else {
            Fixity::Postfix
          },
        )
      })
  }

  fn get_precedence_fixity(
    &self,
    context: &mut ParsingContext,
    fixity: Fixity,
  ) -> Option<Precedence> {
    Some(match self {
      Self::Operand { op, .. } => match (op, &fixity) {
        _ => return None,
      },
      Self::Token(token) => match (token, &fixity) {
        (Token::Identifier(src), Fixity::None) => {
          context.namespace.get(&src).and_then(|decl| match decl {
            Declaration::Variable(_, p) => Some(p),
            _ => None,
          })?
        },
        (
          Token::String(_)
          | Token::Char(_)
          | Token::Number(..)
          | Token::Boolean(_)
          | Token::Placeholder,
          Fixity::None,
        ) => Precedence(None, None),
        (token, Fixity::Prefix) => Precedence(
          None,
          Some(match token {
            Token::Add | Token::Sub => 9,
            Token::Inc | Token::Dec => 11,
            Token::Mult => 13,
            Token::Identifier(src) if src == "not" => 29,
            Token::Async => 36,
            Token::Await => 33,
            Token::Inline => 36,
            Token::Identifier(src) if src == "print" => 0,
            _ => return None,
          }),
        ),
        (token, Fixity::Postfix) => Precedence(
          Some(match token {
            Token::Bang => 15,
            _ => return None,
          }),
          None,
        ),
        (token, Fixity::Infix) => match token {
          Token::LBrace => Precedence(Some(26), Some(27)),
          Token::Period => Precedence(Some(24), Some(23)),
          Token::Equal => Precedence(Some(255), Some(1)),
          Token::ColonEqual => Precedence(Some(255), Some(1)),
          Token::Mod => Precedence(Some(28), Some(29)),
          Token::Add | Token::Sub => Precedence(Some(5), Some(6)),
          Token::Mult | Token::Div => Precedence(Some(7), Some(8)),
          Token::EqualEqual => Precedence(Some(20), Some(19)),
          Token::LAngleBracket => Precedence(Some(20), Some(19)),
          Token::RAngleBracket => Precedence(Some(20), Some(19)),
          Token::LessEqual => Precedence(Some(20), Some(19)),
          Token::GreaterEqual => Precedence(Some(20), Some(19)),
          Token::Arrow => Precedence(Some(37), Some(0)),
          Token::Apply => Precedence(Some(255), Some(1)),
          Token::Is => Precedence(Some(32), Some(33)),

          Token::Identifier(src) if src == "mod" => Precedence(Some(22), Some(21)),
          Token::Identifier(src) if src == "and" => Precedence(Some(24), Some(23)),
          Token::Identifier(src) if src == "or" => Precedence(Some(25), Some(26)),
          _ => return None,
        },
        _ => return None,
      },
    })
  }
}
