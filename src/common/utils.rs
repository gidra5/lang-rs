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
  ($($key: expr => $value: expr),*) => {{
    let mut map = HashMap::new();
    $(
      map.insert($key, $value);
    )*
    map
  }};
}
