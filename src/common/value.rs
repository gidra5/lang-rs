#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Identifier(String),
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  // Func,
  // Struct,
  None,
}

impl Value {
  pub fn add(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left + right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn sub(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left - right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn mult(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left * right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn div(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left / right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn rem(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left % right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn pow(self, right: Value) -> Value {
    use Value::*;
    match self {
      Number(left) => {
        match right {
          Number(right) => Value::Number(left.powf(right)),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  /// Equal
  pub fn e(self, right: Value) -> Value { Value::Boolean(self == right) }

  /// Less or equal
  pub fn le(self, right: Value) -> Value {
    use Value::*;
    match self {
      Value::Number(left) => {
        match right {
          Value::Number(right) => Value::Boolean(left <= right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  /// Greater or equal
  pub fn ge(self, right: Value) -> Value {
    use Value::*;
    match self {
      Value::Number(left) => {
        match right {
          Value::Number(right) => Value::Boolean(left >= right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }

  pub fn neg(self) -> Value {
    use Value::*;
    match self {
      Value::Number(val) => Value::Number(-val),
      _ => Value::None,
    }
  }

  pub fn inv(self) -> Value {
    use Value::*;
    match self {
      Value::Boolean(val) => Value::Boolean(!val),
      _ => Value::None,
    }
  }

  pub fn dec(self) -> Value {
    use Value::*;
    match self {
      Value::Number(val) => Value::Number(val - 1.),
      _ => Value::None,
    }
  }

  pub fn inc(self) -> Value {
    use Value::*;
    match self {
      Value::Number(val) => Value::Number(val + 1.),
      _ => Value::None,
    }
  }
}
