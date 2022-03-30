use super::Type;
use crate::{common::value::Value, dict, dict_enum_type, map, nominal_type, set, type_ref};

#[test]
fn types_void() {
  assert!(Type::Void < Type::Void);
  assert!(Type::Void < Type::String);
  assert!(Type::Void < Type::Unknown);
}

#[test]
fn types_unknown() {
  assert!(Type::Unknown < Type::Unknown);
  assert!(Type::String < Type::Unknown);
}

#[test]
fn types_1() { assert!(!(Type::Boolean < Type::String)) }

#[test]
fn types_type_ref() {
  assert_eq!(
    Type::Void < Type::String,
    type_ref!(Type::Void) < type_ref!(Type::String)
  )
}

#[test]
fn types_nominal_1() {
  let nominal1 = nominal_type!(Type::String);
  let nominal2 = nominal_type!(Type::String);
  // nominal types are equal only to themselves
  assert_ne!(nominal1, Type::String);
  assert_ne!(nominal1, nominal2);
  assert_eq!(nominal1, nominal1);

  // can be used as value for string
  assert!(nominal1 <= Type::String);

  // but string can't be used in place of nominal string
  assert!(!(Type::String <= nominal1));
}

#[test]
fn types_nominal_transitive_subtyping() {
  let nominal1 = nominal_type!(Type::String);
  let nominal2 = nominal_type!(Type::Intersection(set!(Type::Boolean, nominal1.clone())));
  let nominal3 = nominal_type!(Type::Intersection(set!(Type::Number, nominal2.clone())));
  assert!(nominal2 < nominal1);
  assert!(nominal3 < nominal2);
  assert!(nominal3 < nominal1);
}

#[test]
fn types_nominal_multiple_subtyping() {
  let nominal1 = nominal_type!(Type::String);
  let nominal2 = nominal_type!(Type::String);
  let nominal3 = nominal_type!(Type::Intersection(set![
    Type::String,
    nominal1.clone(),
    nominal2.clone()
  ]));
  assert!(nominal3 < nominal2);
  assert!(nominal3 < nominal1);
}

#[test]
fn types_union() {
  let union = Type::Union(set![Type::Boolean, Type::String]);
  assert!(Type::Boolean < union);
  assert!(Type::String < union);
  assert!(!(Type::Char < union));
}

#[test]
fn types_enum() {
  let enum_t_1 = dict_enum_type![bool: Type::Boolean, string: Type::String, char: Type::Char];
  let enum_t_2 = dict_enum_type![bool: Type::Boolean, string: Type::String];
  assert!(!(Type::Boolean < enum_t_1));
  assert!(!(Type::String < enum_t_1));
  assert!(!(Type::Char < enum_t_1));
  assert!(enum_t_2 < enum_t_1);
  assert!(!(enum_t_1 < enum_t_2));
}

#[test]
fn types_intersect() {
  let value_t = Type::Value(Value::String("s".to_string()));
  let intersect = Type::Intersection(set![Type::String, value_t.clone()]);
  assert!(value_t < intersect);
  assert!(!(Type::String < intersect));
}

#[test]
fn types_records_1() {
  let r1 = Type::Record(dict![a: Type::String]);
  let r2 = Type::Record(dict![b: Type::String]);
  assert_eq!(Type::partial_cmp(&r1, &r2), None);
  assert_eq!(Type::partial_cmp(&r2, &r1), None);
}

#[test]
fn types_records_2() {
  let r1 = Type::Record(dict![a: Type::String, c: Type::String]);
  let r2 = Type::Record(dict![b: Type::String, d: Type::String]);
  assert_eq!(Type::partial_cmp(&r1, &r2), None);
  assert_eq!(Type::partial_cmp(&r2, &r1), None);
}

#[test]
fn types_records_3() {
  let r1 = Type::Record(dict![a: Type::String, c: Type::String]);
  let r2 = Type::Record(dict![b: Type::String, c: Type::String]);
  assert_eq!(Type::partial_cmp(&r1, &r2), None);
  assert_eq!(Type::partial_cmp(&r2, &r1), None);
}

#[test]
fn types_records_4() {
  let r1 = Type::Record(dict![a: Type::String, b: Type::String]);
  let r2 = Type::Record(dict![b: Type::String]);
  assert!(r1 < r2);
  assert!(!(r2 < r1));
}

#[test]
fn types_intersect_records() {
  let intersect = Type::Intersection(set![
    Type::Record(dict![a: Type::String]),
    Type::Record(dict![b: Type::String])
  ]);
  assert!(Type::Record(dict![a: Type::String, b: Type::String]) < intersect);
  assert!(!(Type::Record(dict![a: Type::String]) < intersect));
  assert!(!(Type::Record(dict![b: Type::String]) < intersect));
}

#[test]
fn types_empty_intersect() {
  let intersect = Type::Intersection(set![Type::String, Type::Boolean]);
  assert!(Type::Void < intersect);
  assert!(!(Type::String < intersect));
  assert!(!(Type::Boolean < intersect));
}

#[test]
fn types_negated() {
  let negated = Type::Negated(Box::new(Type::String));
  assert!(Type::Boolean < negated);
  assert!(!(Type::String < negated));
}
