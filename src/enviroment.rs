use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Enviroment {
  name:      String,
  value:     Value,
  enclosing: Option<Box<Enviroment>>,
}

pub enum CloseBy {
  Copy,
  Ref,
  Move,
}

impl Enviroment {
  pub fn new(name: String, value: Value) -> Enviroment {
    Enviroment {
      name,
      value,
      enclosing: None,
    }
  }

  pub fn new_with_variables(variables: HashMap<String, Value>) -> Option<Enviroment> {
    variables
      .into_iter()
      .fold(None, |enclosing, (name, value)| {
        Some(match enclosing {
          Some(e) => Enviroment::define(e, name, value),
          None => Enviroment::new(name, value),
        })
      })
  }

  pub fn get_immediate(&self, ident: String) -> Option<Value> {
    if self.name == ident {
      Some(self.value.clone())
    } else {
      None
    }
  }

  fn resolve(self, ident: String) -> Option<Enviroment> { self.resolve_enclosed(ident).1 }

  /// traverses enviroment and when finds requested binding returns env with it
  /// and part of env without it
  fn resolve_enclosed(self, ident: String) -> (Option<Enviroment>, Option<Enviroment>) {
    if self.name == ident {
      (None, Some(self))
    } else {
      match self.enclosing {
        None => (Some(self), None),
        Some(e) => {
          let (enclosing, rest) = e.resolve_enclosed(ident);

          (
            Some(Enviroment {
              enclosing: enclosing.map(Box::new),
              ..self
            }),
            rest,
          )
        },
      }
    }
  }

  /// `resolve_enclosed` and `close_over` are inverses:
  /// ```
  /// let (rest, resolved) = env.resolve_enclosed(name);
  /// assert!(resolved.close_over(rest) == env)
  /// ```
  pub fn close_over(self, env: Enviroment) -> Enviroment {
    Enviroment {
      enclosing: Some(Box::new(
        env
          .enclosing
          .map_or(self.clone(), |box e| self.close_over(e)),
      )),
      ..env
    }
  }

  pub fn get(&self, ident: String) -> Option<Value> { self.clone().resolve(ident).map(|e| e.value) }

  pub fn define(self, name: String, value: Value) -> Enviroment {
    Enviroment {
      name,
      value,
      enclosing: Some(Box::new(self)),
    }
  }

  pub fn remove(self, ident: String) -> (Option<Value>, Option<Enviroment>) {
    let (left, resolved) = self.resolve_enclosed(ident);

    match resolved {
      Some(env) => {
        let (value, enclosing) = (env.value, env.enclosing);
        let env = left.map(|e| Enviroment { enclosing, ..e });
        (Some(value), env)
      },
      None => (None, left),
    }
  }

  pub fn set(self, ident: String, value: Value) -> (Enviroment, Option<String>) {
    let (left, resolved) = self.clone().resolve_enclosed(ident.clone());

    match resolved {
      Some(e) => {
        let enclosing = Enviroment { value, ..e };
        let env = left.map(|env| enclosing.clone().close_over(env));
        (env.unwrap_or(enclosing), None)
      },
      None => (
        self,
        Some(format!("Variable {} is not declared in this scope", ident)),
      ),
    }
  }
}
