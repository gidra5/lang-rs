#[macro_export]
macro_rules! set {
  ($($item: expr),*) => {{
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
    let map = HashMap::new();
    map
  }};
  ($($key: expr => $value: expr),+) => {{
    use std::collections::HashMap;
    let mut map = HashMap::new();
    $(
      map.insert($key, $value);
    )*
    map
  }};
}
