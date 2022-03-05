#[macro_export]
macro_rules! set {
  () => {{
    use std::collections::HashSet;
    HashSet::new()
  }};
  ($($item: expr),+ $(,)?) => {{
    use std::collections::HashSet;
    let mut set = HashSet::new();
    $(
      set.insert($item);
    )*
    set
  }};
}

#[macro_export]
macro_rules! map {
  () => {{
    use std::collections::HashMap;
    HashMap::new()
  }};
  ($($key: expr => $value: expr),* $(,)?) => {{
    use std::collections::HashMap;
    let mut map = HashMap::new();
    $(
      map.insert($key, $value);
    )*
    map
  }};
}

#[macro_export]
macro_rules! map_str {
  ($($key: expr => $value: expr),* $(,)?) => {map!($($key.to_string() => $value),*)};
}

#[macro_export]
macro_rules! dict {
  ($($key:ident: $value:expr),* $(,)?) => {{
    use crate::map_str;
    map_str!($(stringify!($key) => $value),*)
  }};
}

#[macro_export]
macro_rules! unwrap_enum {
  ($item:ident, $variant:tt) => {
    match $item {
      $variant(x) => x,
      _ => panic!("{} was not {}", stringify!($item), stringify!($variant)),
    }
  };
}

#[macro_export]
macro_rules! unwrap_enum_safe {
  ($item:ident, $variant:tt) => {
    match $item {
      $variant(x) => Some(x),
      _ => None,
    }
  };
}
