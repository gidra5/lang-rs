pub trait ReversableIterator {
  type Item: PartialEq;

  /// Returns previous `size` items in iterator
  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>>;

  /// Returns next `size` items in iterator
  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>>;

  /// Returns next `size` items without consuming iterator
  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>>;
  fn pos(&self) -> usize;

  /// Returns previous item
  fn prev(&self) -> Option<Self::Item> { self.prev_ext(1).pop().flatten() }

  /// Returns next item
  fn next(&mut self) -> Option<Self::Item> { self.next_ext(1).pop().flatten() }

  /// Returns next item without consuming iterator (aka peeks next item)
  fn peek(&self) -> Option<Self::Item> { self.peek_ext(1).pop().flatten() }

  fn check(&self, next: Self::Item) -> bool { self.peek() == Some(next) }

  fn is_next(&mut self, next: Self::Item) -> bool {
    if self.check(next) {
      self.next() != None
    } else {
      false
    }
  }

  fn is_not_next(&mut self, next: Self::Item) -> bool {
    if !self.check(next) {
      self.next() != None
    } else {
      false
    }
  }
}

macro_rules! check_any {
  ($self: expr, $next: expr, $($rest: expr),*) => {
    $self.check($next) || check_next_all!($self, $($rest),*)
  };
}

#[derive(Debug, Clone)]
pub struct ReversableStream<T: Clone + PartialEq> {
  data: Vec<T>,
  pos:  usize,
}

impl<T: Clone + PartialEq> ReversableStream<T> {
  pub fn data(&self) -> &Vec<T> { &self.data }

  pub fn new(data: Vec<T>) -> ReversableStream<T> { ReversableStream { data, pos: 0 } }
}

impl<T: Clone + PartialEq> ReversableIterator for ReversableStream<T> {
  type Item = T;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> {
    let mut next_tokens_iter = self.data.iter().skip(self.pos).cloned();
    let next_tokens = (0..size).map(|_| next_tokens_iter.next()).collect();

    self.pos += size;

    next_tokens
  }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    let mut next_tokens_iter = self.data.iter().skip(self.pos).cloned();
    (0..size).map(|_| next_tokens_iter.next()).collect()
  }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    if self.pos < size {
      let mut prev_tokens_iter = self.data.iter().take(self.pos).cloned();
      (0..size).map(|_| prev_tokens_iter.next()).collect()
    } else {
      let mut prev_tokens_iter = self.data.iter().skip(self.pos - size).cloned();
      (0..size).map(|_| prev_tokens_iter.next()).collect()
    }
  }

  fn pos(&self) -> usize { self.pos }
}
