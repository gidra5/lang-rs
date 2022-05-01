#![allow(unused)]
use crate::{common::*, enviroment::*, token::*, value::Value, *};
use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::HashMap,
  fmt::{Display, Formatter},
  hash::{Hash, Hasher},
  rc::Rc,
};

use itertools::Itertools;

pub mod expr;
pub use expr::*;

mod pattern;
pub use pattern::*;

pub mod script;
pub use script::*;

pub mod module;
pub use module::*;

pub mod items;
pub use items::*;

pub mod inline;
pub use inline::*;


impl Display for ParsingError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Generic(msg) => write!(f, "{}", msg),
      Self::Aggregate(errs) => write!(f, "{}", errs.into_iter().join("\n")),
    }
  }
}
