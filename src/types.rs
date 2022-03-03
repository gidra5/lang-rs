use std::{
  cmp::Ordering,
  collections::{HashMap, HashSet},
  rc::{Rc, Weak},
};

use crate::{
  ast::{Expression, ParsingContext, Precedence},
  common::value::Value,
  map,
};

macro_rules! is_unit_type {
  ($val_type:ident) => {
    matches!(
      $val_type,
      Type::Value(_) | Type::String | Type::Number | Type::Char | Type::Boolean
    )
  };
}

/// A type of any variable in program
/// When checking type of an value it is verified in structural manner
#[derive(Clone, Debug, Eq)]
pub enum Type {
  Void, // aka empty set

  Value(Value),
  String,
  Number,
  Char,
  Boolean,

  Tuple(Vec<Type>),
  Record(HashMap<String, Type>),
  Union(HashSet<Type>),
  Enum(HashMap<String, Type>),
  Function(Box<Type>, Box<Type>),

  NominalType {
    _type:      Rc<Type>,
    supertypes: Vec<Rc<Type>>,
  },

  Unknown, // aka any type, universum
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Value(l0), Self::Value(r0)) => l0 == r0,
      (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
      (Self::Record(l0), Self::Record(r0)) => l0 == r0,
      (Self::Union(l0), Self::Union(r0)) => l0.is_subset(r0) && r0.is_subset(l0),
      (Self::Enum(l0), Self::Enum(r0)) => l0 == r0,
      (Self::Function(l0, l1), Self::Function(r0, r1)) => l0 == r0 && l1 == r1,

      // Nominal types are considered equal only if they refer to the same type
      (Self::NominalType { _type: l0, .. }, Self::NominalType { _type: r0, .. }) => {
        Rc::ptr_eq(l0, r0)
      },

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

impl PartialOrd for Type {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(match (self, other) {
      (Type::Value(Value::String(_)), Type::String) => Ordering::Less,
      (Type::Value(Value::Number(_)), Type::Number) => Ordering::Less,
      (Type::Value(Value::Char(_)), Type::Char) => Ordering::Less,
      (Type::Value(Value::Boolean(_)), Type::Boolean) => Ordering::Less,

      (Type::Void, _) => Ordering::Less,

      // tuple1 is subtype of tuple2 if for every item1 in tuple2 there is an item2 in tuple1 in the
      // same position such that item1 is subtype of item2
      (Type::Tuple(items1), Type::Tuple(items2))
        if items1.len() >= items2.len()
          && items2
            .iter()
            .enumerate()
            .all(|(var, var_type)| var_type < items1.get(var).unwrap_or_default()) =>
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
            .all(|(var, var_type)| var_type < items1.get(var).unwrap_or_default()) =>
      {
        Ordering::Less
      },
      // if record has one element it is equivalent to just that item, so du comparsion on that
      (t1, Type::Record(items)) if items.len() == 1 => {
        Self::partial_cmp(t1, items.values().next().unwrap())?
      },

      // union1 is subtype of union2 if for every var1 in union1 there is a var2 in union2 such that
      // var1 is subtype of var2
      (Type::Union(vars1), Type::Union(vars2))
        if vars2.len() >= vars1.len()
          && vars1
            .iter()
            .all(|var1| vars2.iter().any(|var2| var1 < var2)) =>
      {
        Ordering::Less
      },
      (t, Type::Union(vars)) if vars.iter().any(|var2| t < var2) => Ordering::Less,

      // enum1 is subtype of enum2 if for all var2 from enum2 there is var1 in enum1 with the same
      // name such that var1 < var2
      (Type::Enum(vars1), Type::Enum(vars2))
        if vars1
          .iter()
          .all(|(var, var_type)| var_type < vars2.get(var).unwrap_or_default()) =>
      {
        Ordering::Less
      },

      // function1 is subtype of function2 if arg2 is subtype of arg1 and res1 is subtype of res2
      (Type::Function(box arg1, box res1), Type::Function(box arg2, box res2))
        if arg2 < arg1 && res1 < res2 =>
      {
        Ordering::Less
      },

      // any type is subtype of unknown and unknown isn't subtype of anything exept itself
      (_, Type::Unknown) => Ordering::Less,

      // nominal types can have declared supertypes, and so they can be in relation only with those
      // and itself
      (
        Type::NominalType {
          _type: t1,
          supertypes: st1,
        },
        Type::NominalType {
          _type: t2,
          supertypes: st2,
        },
      ) => {
        if st1.contains(t2) {
          Ordering::Less
        } else if st2.contains(t1) {
          Ordering::Greater
        } else if t1 == t2 {
          Ordering::Equal
        } else {
          return None;
        }
      },
      (Type::NominalType { _type: t1, .. }, t2) => Self::partial_cmp(t1, t2)?,

      (t1, t2) if t1 == t2 => Ordering::Equal,
      (t1, t2) if t1 > t2 => Ordering::Greater,
      _ => return None,
    })
  }
}

impl std::hash::Hash for Type {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
}

impl Type {
  fn of_expr(expr: &Expression, context: &ParsingContext) -> Type {
    match expr {
      Expression::Value(token) => {
        match token {
          _ => todo!(),
        }
      },
      Expression::Record(r) => todo!(),
      Expression::Block(stmts) => todo!(),
      Expression::If(_, box t_b, f_b) => {
        Type::Enum(
          map!["True".to_string() => Type::of_expr(t_b, context), "False".to_string() => if let Some(box f_b) = f_b { Type::of_expr(f_b, context) } else { Type::Void }],
        )
      },
      Expression::For(_, _, box body) => Type::of_expr(body, context),
      Expression::Prefix { op, right } => {
        if let Type::Function(box arg, box res) = Type::of_expr(op, context) {
          if Type::of_expr(right, context) < arg {
            res
          } else {
            Type::Void
          }
        } else {
          Type::Void
        }
      },
      Expression::Postfix { left, op } => {
        if let Type::Function(box arg, box res) = Type::of_expr(op, context) {
          if Type::of_expr(left, context) < arg {
            res
          } else {
            Type::Void
          }
        } else {
          Type::Void
        }
      },
      Expression::Infix { left, op, right } => todo!(),
      _ => Type::Void,
    }
  }
  fn of_value(value: &Value) -> Type {
    match value {
      val @ (Value::String(_)
      | Value::Type(_)
      | Value::Boolean(_)
      | Value::Char(_)
      | Value::Number(_)) => Type::Value(val.clone()),
      Value::EnumValue(x, y) => Type::Enum(map![x.clone() => Type::of_value(y)]),
      Value::Tuple(_) => todo!(),
      Value::Map(_) => todo!(),
      Value::Record(r) => todo!(),
      Value::Function(arg, _, expr) => todo!(),
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
  fn declare(&mut self, name: String, decl_type: Type) {
    self.declare_with_precedence(name, decl_type, None);
  }

  fn declare_with_precedence(&mut self, name: String, decl_type: Type, precedence: Precedence) {
    self
      .0
      .insert(name, Declaration::Variable(decl_type, precedence));
  }

  fn declare_namespace(&mut self, name: String, namespace: Rc<Namespace>) {
    self.0.insert(name, Declaration::Namespace(namespace));
  }

  fn get(&self, name: String) -> Option<Declaration> { self.0.get(&name).cloned() }

  fn get_by_path(&self, path: Vec<String>) -> Option<Declaration> {
    let mut path_iter = path.into_iter();
    let mut item = None;

    let name = path_iter.next();
    if name.is_none() {
      return item;
    }

    let name = name.unwrap();
    item = self.get(name);
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
      item = namespace.get(name);

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
