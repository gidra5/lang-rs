#[derive(Clone, Debug)]
pub struct Buffered<I: Iterator> {
  pub iterator: I,
  pub buffer:   Vec<I::Item>,
  pos:          usize,
}

pub trait Buf<T: Iterator> {
  fn buffered(self) -> Buffered<T>;
}

impl<T: Iterator> Buf<T> for T {
  fn buffered(self) -> Buffered<T> {
    Buffered {
      iterator: self,
      buffer:   vec![],
      pos:      0,
    }
  }
}

impl<T: Iterator> Iterator for Buffered<T>
where
  T::Item: Clone,
{
  type Item = T::Item;
  fn next(&mut self) -> Option<T::Item> { self.next_ext(1).pop().flatten() }
}


impl<T: Iterator> Buffered<T>
where
  T::Item: Clone,
{
  /// Returns next `size` items in iterator
  pub fn next_ext(&mut self, size: usize) -> Vec<Option<T::Item>> {
    let res = self.peek_ext(size);
    self.pos = (self.pos + size);
    res
  }

  /// Returns next `size` items without consuming iterator
  pub fn peek_ext(&mut self, size: usize) -> Vec<Option<T::Item>> {
    let pos = self.pos;
    let to_take = (self.pos + size) - self.buffer.len();

    for i in (0..to_take) {
      if let Some(item) = self.iterator.next() {
        self.buffer.push(item)
      } else {
        break;
      }
    }

    (pos..pos + size)
      .map(|i| self.buffer.get(i).cloned())
      .collect()
  }

  /// Returns previous `size` items in iterator
  pub fn prev_ext(&self, size: usize) -> Vec<Option<T::Item>> {
    (self.pos - size..self.pos)
      .map(|i| self.buffer.get(i).cloned())
      .collect()
  }

  /// backtrack iterator `size` steps back
  pub fn backtrack_ext(&mut self, size: usize) -> Vec<Option<T::Item>> {
    let res = self.prev_ext(size);
    self.pos = (self.pos - size).max(0);
    res
  }

  /// Returns next item without consuming iterator (aka peeks next item)
  pub fn peek(&mut self) -> Option<T::Item> { self.peek_ext(1).pop().flatten() }

  /// Returns previous item
  pub fn prev(&self) -> Option<T::Item> { self.prev_ext(1).pop().flatten() }

  pub fn backtrack(&mut self) -> Option<T::Item> { self.backtrack_ext(1).pop().flatten() }

  pub fn pos(&self) -> usize { self.pos }
}

// macro_rules! equations_impl {
//   (() -> (/* final output */)) => {
//       // do something with final output
//   };
//   ((/* pattern a */ $($rest:tt)*) -> ($($output:tt)*)) => {
//       equations_impl!(($($rest)*) -> ($(output)*) /* new output */ );
//   };
//   ((/* pattern b */ $($rest:tt)*) -> (/* current output */)) => {
//       equations_impl!(($($rest)*) -> ($(output)*) /* new output */ );
//   };
// }

#[macro_export]
macro_rules! is_next {
  ([skip_expr] $self:ident, $($rest: tt)*) => {
    $self.next()
  };
  ([skip_expr] $self:ident[$n: expr], $($rest: tt)*) => {
    $self.next_ext($n)
  };
  ([skip $($rest2: tt)*] $($rest: tt)*) => {{
    let res = is_next!([$($rest2)*] $($rest)*);
    if res {
      is_next!([skip_expr] $($rest)*);
    }
    res
  }};
  ([] $($rest: tt)*) => {
    is_next!($($rest)*)
  };
  ([not $($rest2: tt)*] $($rest: tt)*) => {
    !is_next!([$($rest2)*] $($rest)*)
  };
  ($self:ident, $item:pat) => {
    if let Some($item) = $self.peek() {
      true
    } else {
      false
    }
  };
  ($self:ident[$n: expr], $($item:pat),* $(,)?) => {
    if let [$(Some($item)),*] = $self.peek_ext($n)[..] {
      true
    } else {
      false
    }
  };
}
