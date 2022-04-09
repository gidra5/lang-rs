use std::path::Path;

use crate::{
  ast::{Module, ModuleItem, Script},
  common::{str_parse, str_parse_file, tests::expr},
};

fn test_s(path: &str) -> Result<Script, String> {
  let filepath = Path::new(file!())
    .parent()
    .unwrap()
    .join("../../samples")
    .join(path);

  str_parse_file::<Script>(filepath.to_str().unwrap())
}

fn test_m(path: &str) -> Result<Module, String> {
  let filepath = Path::new(file!())
    .parent()
    .unwrap()
    .join("../../samples")
    .join(path);

  str_parse_file::<Module>(filepath.to_str().unwrap())
}

#[test]
fn module_1() {
  assert_eq!(
    test_m("sample_mod1/module.m.lang"),
    Ok(Module {
      _pub:    vec![
        str_parse::<ModuleItem>("let x = 1").unwrap(),
        str_parse::<ModuleItem>("let y = () => 1").unwrap()
      ],
      private: vec![],
      entry:   None,
    })
  );
  assert_eq!(
    test_s("sample_mod1/script.s.lang"),
    Ok(Script(
      vec![
        expr("print module.x").unwrap(),
        expr("print module.y").unwrap(),
        expr("print module").unwrap()
      ],
      Module {
        _pub:    vec![],
        private: vec![str_parse::<ModuleItem>("use \"./module.m.lang\" as module").unwrap()],
        entry:   None,
      }
    ))
  )
}

#[test]
fn module_2() {
  assert_eq!(
    test_m("sample_mod2/module.m.lang"),
    Ok(Module {
      _pub:    vec![
        str_parse::<ModuleItem>("let x = 1").unwrap(),
        str_parse::<ModuleItem>("let y = () => z").unwrap()
      ],
      private: vec![str_parse::<ModuleItem>("ext z as number").unwrap(),],
      entry:   None,
    })
  );
  assert_eq!(
    test_s("sample_mod2/script.s.lang"),
    Ok(Script(
      vec![
        expr("print module.x").unwrap(),
        expr("print module.y").unwrap(),
        expr("print module").unwrap()
      ],
      Module {
        _pub:    vec![],
        private: vec![
          str_parse::<ModuleItem>("use \"./module.m.lang\" as module with (z: 2)").unwrap()
        ],
        entry:   None,
      }
    ))
  )
}

#[test]
fn module_3() {
  assert_eq!(
    test_m("sample_mod3/module2.m.lang"),
    Ok(Module {
      _pub:    vec![str_parse::<ModuleItem>("let b = 3").unwrap(),],
      private: vec![],
      entry:   None,
    })
  );
  assert_eq!(
    test_m("sample_mod3/module.m.lang"),
    Ok(Module {
      _pub:    vec![
        str_parse::<ModuleItem>("let x = b").unwrap(),
        str_parse::<ModuleItem>("let y = () => z").unwrap()
      ],
      private: vec![
        str_parse::<ModuleItem>("use \"./module2.m.lang\" as (b)").unwrap(),
        str_parse::<ModuleItem>("ext z as number").unwrap()
      ],
      entry:   None,
    })
  );
  assert_eq!(
    test_s("sample_mod3/script.s.lang"),
    Ok(Script(
      vec![
        expr("print module.x").unwrap(),
        expr("print module.y").unwrap(),
        expr("print module").unwrap()
      ],
      Module {
        _pub:    vec![],
        private: vec![
          str_parse::<ModuleItem>("use \"./module.m.lang\" as module with (z: 2)").unwrap()
        ],
        entry:   None,
      }
    ))
  )
}

#[test]
fn module_4() {
  assert_eq!(
    test_m("sample_mod4/module2.m.lang"),
    Ok(Module {
      _pub:    vec![str_parse::<ModuleItem>("let b = 3").unwrap(),],
      private: vec![],
      entry:   None,
    })
  );
  assert_eq!(
    test_m("sample_mod4/module.m.lang"),
    Ok(Module {
      _pub:    vec![
        str_parse::<ModuleItem>("use \"./module2.m.lang\" as inner").unwrap(),
        str_parse::<ModuleItem>("let x = inner.b").unwrap(),
        str_parse::<ModuleItem>("let y = () => z").unwrap()
      ],
      private: vec![str_parse::<ModuleItem>("ext z as number").unwrap()],
      entry:   None,
    })
  );
  assert_eq!(
    test_s("sample_mod4/script.s.lang"),
    Ok(Script(
      vec![
        expr("print module.x").unwrap(),
        expr("print module.y").unwrap(),
        expr("print module.inner").unwrap(),
        expr("print module").unwrap()
      ],
      Module {
        _pub:    vec![],
        private: vec![
          str_parse::<ModuleItem>("use \"./module.m.lang\" as module with (z: 2)").unwrap()
        ],
        entry:   None,
      }
    ))
  )
}

#[test]
fn module_5() {
  assert_eq!(
    test_m("sample_mod5/module2.m.lang"),
    Ok(Module {
      _pub:    vec![str_parse::<ModuleItem>("let b = 3").unwrap(),],
      private: vec![],
      entry:   None,
    })
  );
  assert_eq!(
    test_m("sample_mod5/module.m.lang"),
    Ok(Module {
      _pub:    vec![
        str_parse::<ModuleItem>("use \"./module2.m.lang\" as (b)").unwrap(),
        str_parse::<ModuleItem>("let x = b").unwrap(),
        str_parse::<ModuleItem>("let y = () => z").unwrap()
      ],
      private: vec![str_parse::<ModuleItem>("ext z as number").unwrap()],
      entry:   None,
    })
  );
  assert_eq!(
    test_s("sample_mod5/script.s.lang"),
    Ok(Script(
      vec![
        expr("print module.x").unwrap(),
        expr("print module.y").unwrap(),
        expr("print module.b").unwrap(),
        expr("print module").unwrap()
      ],
      Module {
        _pub:    vec![],
        private: vec![
          str_parse::<ModuleItem>("use \"./module.m.lang\" as module with (z: 2)").unwrap()
        ],
        entry:   None,
      }
    ))
  )
}
