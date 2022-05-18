#[derive(Clone, Debug)]
pub struct GroupMap<T, I, F>
where
  I: Iterator,
  F: FnMut(&mut I) -> Option<T>,
{
  pub iterator: I,
  grouper:      F,
}

pub trait GroupMapTrait<T, I>
where
  I: Iterator,
{
  fn group_map<F: FnMut(&mut I) -> Option<T>>(self, f: F) -> GroupMap<T, I, F>;
}

impl<T, I: Iterator> GroupMapTrait<T, I> for I {
  fn group_map<F: FnMut(&mut I) -> Option<T>>(self, f: F) -> GroupMap<T, I, F> {
    GroupMap {
      iterator: self,
      grouper:  f,
    }
  }
}

impl<T, I, F> Iterator for GroupMap<T, I, F>
where
  I: Iterator,
  F: FnMut(&mut I) -> Option<T>,
{
  type Item = T;
  fn next(&mut self) -> Option<T> { (self.grouper)(&mut self.iterator) }
}
