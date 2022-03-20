use std::cmp::Ordering;

use crate::{
  ast::{Expression, ParsingContext},
  token::{Token, TokenExt},
  token_pat,
};



#[derive(Clone, PartialEq, Default, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,

  #[default]
  None,
}

pub type Precedence = (Option<u8>, Option<u8>);

#[derive(PartialEq, Default, Debug, Clone)]
pub struct Operator {
  pub op:         Expression,
  pub precedence: Option<Precedence>,
}

impl PartialOrd for Operator {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    // aka r_bp1 < l_bp2
    Some(match (self.precedence, other.precedence) {
      (Some((_, r_bp1)), Some((l_bp2, _))) => match (r_bp1, l_bp2) {
        (None, None) => Ordering::Greater,
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Less,
        (Some(r_bp1), Some(l_bp2)) => match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
          (false, false) => Ordering::Equal,
          (true, false) => Ordering::Less,
          (false, true) => Ordering::Greater,
          _ => return None,
        },
      },
      (x, y) => x.partial_cmp(&y)?,
    })
  }
}

impl Operator {
  pub fn ensure_exists(self) -> Option<Self> {
    Some(
      if self.precedence == None {
        return None;
      } else {
        self
      },
    )
  }

  pub fn convert(self, context: &mut ParsingContext) -> Self {
    match self {
      Self {
        op: Expression::Value(token),
        ..
      } => Self::new(token, context, false),
      x => x,
    }
  }

  pub fn new(token: TokenExt, context: &mut ParsingContext, prefix: bool) -> Self {
    let precedence = Self::get_precedence(
      &token,
      context,
      if prefix {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    )
    .or_else(|| {
      Self::get_precedence(
        &token,
        context,
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

  fn get_precedence(
    token: &TokenExt,
    context: &mut ParsingContext,
    fixity: Fixity,
  ) -> Option<Precedence> {
    Some(match (&token, &fixity) {
      // (token_pat!(token: Identifier, src), Fixity::None) => {
      //   context.namespace.get(src).and_then(|decl| {
      //     match decl {
      //       Declaration::Variable(t, p) => Some(p),
      //       _ => None,
      //     }
      //   })?
      // },
      (
        token_pat!(token: Identifier | String | Placeholder | Char | Number | Boolean),
        Fixity::None,
      ) => (None, None),
      (token_pat!(token, src), Fixity::Prefix) => (
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
          Token::Identifier if src == "let" => 254,
          _ => return None,
        }),
      ),
      (token_pat!(token, src), Fixity::Postfix) => (
        Some(match token {
          Token::Bang => 15,
          _ => return None,
        }),
        None,
      ),
      (token_pat!(token, src), Fixity::Infix) => match token {
        Token::LBrace => (Some(26), Some(27)),
        Token::Period => (Some(24), Some(23)),
        Token::Equal => (Some(255), Some(1)),
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
      },
      _ => return None,
    })
  }
}
