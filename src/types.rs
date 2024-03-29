use std::{
  cmp::Ordering,
  collections::{HashMap, HashSet},
  rc::{Rc, Weak},
};

use itertools::Itertools;

use crate::{
  ast::{Expression, ParsingContext, ParsingError, Pattern, PatternWithDefault, Precedence},
  common::value::Value,
  set,
  token_pat,
};

#[path = "tests/types.rs"]
mod tests;

#[macro_export]
macro_rules! is_unit_type {
  ($val_type:ident) => {
    matches!(
      $val_type,
      Type::Value(_) | Type::String | Type::Number | Type::Char | Type::Boolean
    )
  };
}

#[macro_export]
macro_rules! type_ref {
  ($t:expr) => {
    Type::TypeRef(std::rc::Rc::new($t))
  };
}

#[macro_export]
macro_rules! nominal_type {
  ($t:expr) => {
    Type::NominalType(std::rc::Rc::new($t))
  };
}

#[macro_export]
macro_rules! enum_type {
  ($($key: expr => $_type:expr), *) => {
    Type::Union(set![$(Type::Tuple(vec![$key, $_type])),*])
  };
}

#[macro_export]
macro_rules! dict_enum_type {
  ($($key:ident: $_type:expr), *) => {
    Type::Union(set![$(
      Type::Tuple(vec![
        Type::Value(Value::String(
          stringify!($key).to_string()
        )),
        $_type
      ])
    ),*])
  };
}

pub type TypeRef = Rc<Type>;

/// A type of any variable in program
///
/// When checking type of an value it is verified in structural manner
#[derive(Clone, Debug, Eq)]
pub enum Type {
  Void, // aka empty set

  Value(Value),

  // a type for every variant of Value
  String,
  Number,
  Char,
  Boolean,
  Symbol,
  Type,
  Tuple(Vec<Type>),              // replace with pair and syntax sugar?
  Record(HashMap<String, Type>), // maybe just use tuple of pairs?

  Union(HashSet<Type>),
  Intersection(HashSet<Type>),
  Negated(Box<Type>),
  Function(String, Box<Type>, Box<Type>), // dependent function type

  TypeRef(TypeRef),
  NominalType(TypeRef), // maybe somehow replace with combination of value::symbol and type?
  TypeOf(String),       // type of some variable in scope

  Unknown, // aka any type, universum
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Value(l0), Self::Value(r0)) => l0 == r0,
      (Self::TypeOf(l0), Self::TypeOf(r0)) => l0 == r0,
      (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
      (Self::Record(l0), Self::Record(r0)) => l0 == r0,
      (Self::Union(l0), Self::Union(r0)) => l0.is_subset(r0) && r0.is_subset(l0),
      (Self::Intersection(l0), Self::Intersection(r0)) => l0.is_subset(r0) && r0.is_subset(l0),
      (Self::TypeRef(l0), Self::TypeRef(r0)) => l0 == r0,
      (Self::Negated(l0), Self::Negated(r0)) => l0 == r0,
      (Self::Function(_, l0, l1), Self::Function(_, r0, r1)) => l0 == r0 && l1 == r1,

      // Nominal types are considered equal only if they refer to the same type
      (Self::NominalType(l0), Self::NominalType(r0)) => Rc::ptr_eq(l0, r0),

      _ => core::mem::discriminant(self) == core::mem::discriminant(other),
    }
  }
}

impl Default for Type {
  fn default() -> Self { Type::Void }
}

impl Default for &Type {
  fn default() -> Self { &Type::Void }
}

/// define subtyping relation as partial order on types,
/// where A <= B means A is subtype of B (bc all types are subtypes of
/// themselves)
impl PartialOrd for Type {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(match (self, other) {
      (Type::Value(Value::String(_)), Type::String) => Ordering::Less,
      (Type::Value(Value::Number(_)), Type::Number) => Ordering::Less,
      (Type::Value(Value::Char(_)), Type::Char) => Ordering::Less,
      (Type::Value(Value::Boolean(_)), Type::Boolean) => Ordering::Less,
      (Type::Value(Value::Type(_)), Type::Type) => Ordering::Less,
      (Type::Value(Value::Symbol(_)), Type::Symbol) => Ordering::Less,
      (Type::Value(Value::Type(box t1)), t2) if t1 < t2 => Ordering::Less,

      (Type::Void, _) => Ordering::Less,

      // tuple1 is subtype of tuple2 if for every item1 in tuple2 there is an item2 in tuple1 in the
      // same position such that item1 is subtype of item2
      (Type::Tuple(items1), Type::Tuple(items2))
        if items1.len() >= items2.len()
          && !items1
            .iter()
            .enumerate()
            .any(|(var, var_type)| items2.get(var).unwrap_or_default() < var_type) =>
      {
        Ordering::Less
      },
      // if tuple has one element it is equivalent to just that item, so du comparsion on that
      (t1, Type::Tuple(items)) if items.len() == 1 => Self::partial_cmp(t1, &items[0])?,

      // record1 is subtype of record2 if for every item1 in record2 there is an item2 in record1 in
      // the same field such that item1 is subtype of item2
      (Type::Record(items1), Type::Record(items2))
        if items1.len() >= items2.len()
          && items2
            .iter()
            .all(|(var, var_type)| items1.get(var).map_or(false, |var| var_type <= var)) =>
      {
        Ordering::Less
      },
      (Type::Record(_), Type::Record(_)) => return None,
      // if record has one element it is equivalent to just that item, so do comparsion on that
      (t1, Type::Record(items)) if items.len() == 1 => {
        Self::partial_cmp(t1, items.values().next().unwrap())?
      },

      // union1 is strict subtype of union2 if there is no var1 in vars1 such that there is no var2
      // in vars2 such that var1 is strict subtype of var2
      (Type::Union(vars1), Type::Union(vars2))
        if !vars1
          .iter()
          .any(|var1| !vars2.iter().any(|var2| var1 < var2)) =>
      {
        Ordering::Less
      },
      (t, Type::Union(vars)) if !vars.iter().all(|var2| !(t < var2)) => Ordering::Less,

      // intersect1 is subtype of intersect2 if for all item2 in intersect2 there is item1 in
      // intersect1 such that item1 is subtype of item2
      (Type::Intersection(items1), Type::Intersection(items2))
        if items2
          .iter()
          .all(|item2| items1.iter().any(|item1| item1 <= item2)) =>
      {
        Ordering::Less
      },
      // type is subtype of intersection if type is subtype of every item in intersection
      (t, Type::Intersection(items)) if items.iter().all(|item| t <= item) => Ordering::Less,

      // function1 is subtype of function2 if arg2 is subtype of arg1 and res1 is subtype of res2
      (Type::Function(_, box arg1, box res1), Type::Function(_, box arg2, box res2))
        if arg2 == arg1 && res1 == res2 =>
      {
        Ordering::Equal
      },
      (Type::Function(_, box arg1, box res1), Type::Function(_, box arg2, box res2))
        if arg2 <= arg1 && res1 <= res2 =>
      {
        Ordering::Less
      },
      (_, Type::Function(_, _, _)) => return None,

      // any type is subtype of unknown and unknown isn't subtype of anything exept itself
      (_, Type::Unknown) => Ordering::Less,

      (t1 @ Type::NominalType(_), t2 @ Type::NominalType(_)) => {
        if t1 == t2 {
          Ordering::Equal
        } else {
          return None;
        }
      },
      (Type::NominalType(t1), t2) => Self::partial_cmp(t1, t2)?,

      (t1, Type::TypeRef(t2)) => Self::partial_cmp(t1, t2)?,

      (t1, Type::Negated(t2)) if !(t1 <= t2) => Ordering::Less,

      (t1, t2) if t1 == t2 => Ordering::Equal,
      (Type::Value(v1), Type::Value(v2)) if v1 == v2 => Ordering::Equal,
      (_, Type::String | Type::Number | Type::Char | Type::Boolean | Type::Value(_)) => {
        return None
      },
      (t1, t2) if t2 < t1 => Ordering::Greater,
      _ => return None,
    })
  }
}

impl std::hash::Hash for Type {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
}

impl Type {
  pub fn to_ref_type(self) -> Type { Type::TypeRef(Rc::new(self)) }
  pub fn of_pat(pat: &PatternWithDefault, _context: &ParsingContext) -> Type {
    match pat.pattern {
      Pattern::Record(_) => todo!(),
      _ => Type::Void,
    }
  }
  pub fn of_expr(expr: &Expression, context: &ParsingContext) -> Result<Self, ParsingError> {
    Ok(match expr {
      Expression::Value(token) => match token {
        token_pat!(token: Add) => Type::Union(set![
          Type::Function(
            "".to_string(),
            Box::new(Type::Tuple(vec![Type::Number, Type::Number])),
            Box::new(Type::Number),
          ),
          Type::Function(
            "".to_string(),
            Box::new(Type::Tuple(vec![Type::String, Type::String])),
            Box::new(Type::String),
          ),
          Type::Function(
            "".to_string(),
            Box::new(Type::Tuple(vec![Type::String, Type::Char])),
            Box::new(Type::String),
          ),
          Type::Function(
            "".to_string(),
            Box::new(Type::Tuple(vec![Type::Char, Type::String])),
            Box::new(Type::String),
          )
        ]),
        t @ token_pat!(token: Number | String | Char | Boolean) => Type::Value(t.value()),
        token_pat!(token: Identifier, src) if src == "string" => Type::String,
        token_pat!(token: Identifier, src) if src == "number" => Type::Number,
        token_pat!(token: Identifier, src) if src == "char" => Type::Char,
        token_pat!(token: Identifier, src) if src == "boolean" => Type::Boolean,
        token_pat!(token: Identifier, src) => {
          context
            .namespace
            .get(src)
            .map_or(Type::Void, |decl| match decl {
              Declaration::Variable(t, _) => t,
              _ => Type::Void,
            })
        },
        _ => todo!(),
      },
      Expression::Record(_) => todo!(),
      Expression::Block(exprs) => exprs
        .last()
        .map_or(Ok(Type::Void), |expr| Type::of_expr(expr, context))?,
      Expression::If(_, box t_b, f_b) => {
        enum_type!(
          Type::of_value(&Value::Boolean(true)) => Type::of_expr(t_b, context)?,
          Type::of_value(&Value::Boolean(false)) => {
            if let Some(box f_b) = f_b { Type::of_expr(f_b, context)? }
            else { Type::Void }
          }
        )
      },
      Expression::For(_, _, box body) => Type::of_expr(body, context)?,
      Expression::Prefix { op, right } => {
        if let Type::Function(_, box arg, box res) = Type::of_expr(op, context)? {
          if Type::of_expr(right, context)? <= arg {
            res
          } else {
            Type::Void
          }
        } else {
          Type::Void
        }
      },
      Expression::Postfix { left, op } => {
        if let Type::Function(_, box arg, box res) = Type::of_expr(op, context)? {
          if Type::of_expr(left, context)? <= arg {
            res
          } else {
            Type::Void
          }
        } else {
          Type::Void
        }
      },
      Expression::Infix { left, op, right } => match op {
        box Expression::Value(token_pat!(token: Arrow)) => Type::Function(
          "".to_string(),
          Box::new(Type::of_pat(
            &PatternWithDefault::from_expr(left)?,
            &context,
          )),
          Box::new(Type::of_expr(right, &context)?),
        ),
        op => {
          if let Type::Function(_, box arg, box res) = Type::of_expr(op, context)? {
            if Type::Tuple(vec![
              Type::of_expr(left, context)?,
              Type::of_expr(right, context)?,
            ]) <= arg
            {
              return Ok(res);
            }
          }
          Type::Void
        },
      },
    })
  }
  pub fn of_value(value: &Value) -> Type {
    match value {
      val @ (Value::String(_)
      | Value::Type(_)
      | Value::Boolean(_)
      | Value::Char(_)
      | Value::Number(_)) => Type::Value(val.clone()),
      Value::Tuple(values) => Type::Tuple(
        values
          .iter()
          .map(|value| Type::of_value(value))
          .collect_vec(),
      ),
      Value::Map(values) => Type::Union(
        values
          .iter()
          .map(|(key, value)| {
            Type::Function(
              "".to_string(),
              Box::new(Type::of_value(key)),
              Box::new(Type::of_value(value)),
            )
          })
          .collect(),
      ),
      Value::Record(values) => Type::Record(
        values
          .iter()
          .map(|(key, value)| (key.clone(), Type::of_value(value)))
          .collect(),
      ),
      Value::Function(arg, env, expr) => {
        let context = ParsingContext::from_env(env);
        Type::Function(
          "".to_string(),
          Box::new(Type::of_pat(
            &PatternWithDefault::from_expr(arg).unwrap(),
            &context,
          )),
          Box::new(Type::of_expr(expr, &context).unwrap()),
        )
      },
      _ => Type::Void,
    }
  }
}

pub struct Namespace(pub HashMap<String, Declaration>);

#[derive(Clone)]
pub enum Declaration {
  Variable(Type, Precedence),
  Namespace(Rc<Namespace>),
  ImportedNamespace(Weak<Namespace>),
}

impl Namespace {
  pub fn declare(&mut self, name: String, decl_type: Type) {
    self.declare_with_precedence(name, decl_type, (None, None));
  }

  pub fn declare_with_precedence(&mut self, name: String, decl_type: Type, precedence: Precedence) {
    self
      .0
      .insert(name, Declaration::Variable(decl_type, precedence));
  }

  pub fn declare_namespace(&mut self, name: String, namespace: Rc<Namespace>) {
    self.0.insert(name, Declaration::Namespace(namespace));
  }

  pub fn get(&self, name: &String) -> Option<Declaration> { self.0.get(name).cloned() }

  pub fn get_by_path(&self, path: Vec<String>) -> Option<Declaration> {
    let mut path_iter = path.into_iter();
    let mut item = None;

    let name = path_iter.next();
    if name.is_none() {
      return item;
    }

    let name = name.unwrap();
    item = self.get(&name);
    let mut namespace = if let Some(Declaration::Namespace(ref n)) = item {
      Rc::clone(n)
    } else if let Some(Declaration::ImportedNamespace(ref item)) = item {
      if let Some(n) = item.upgrade() {
        n
      } else {
        unreachable!()
      }
    } else {
      if path_iter.peekable().peek().is_none() || item.is_none() {
        return item;
      } else {
        unreachable!()
      }
    };

    loop {
      let name = path_iter.next();
      if name.is_none() || item.is_none() {
        break item;
      }

      let name = name.unwrap();
      item = namespace.get(&name);

      if let Some(Declaration::Namespace(ref n)) = item {
        namespace = Rc::clone(n)
      } else if let Some(Declaration::ImportedNamespace(ref item)) = item {
        if let Some(n) = item.upgrade() {
          namespace = n
        }
      }
    }
  }
}
