#[derive(Clone, Debug, PartialEq)]
pub struct Script(pub Vec<Expression>, pub Module);

impl Parseable for Script {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let mut exprs = vec![];
    let mut module = Module::default();
    let mut errors = vec![];

    loop {
      if check_token_end!(stream) {
        if errors.len() == 0 {
          return Ok(Self(exprs, module));
        } else {
          return Err(ParsingError::Aggregate(errors));
        }
      }

      match stream.peek() {
        match_token!(Import | External) => {
          match ModuleItem::parse(stream, context) {
            Ok(item) => module.private.push(item),
            Err(err) => errors.push(err),
          };
        },
        match_token!(Public) => {
          match ModuleItem::parse(stream, context) {
            Ok(item) => module._pub.push(item),
            Err(err) => errors.push(err),
          };
        },
        _ => break,
      }

      skip!(stream, Semicolon | NewLine);
    }

    loop {
      if check_token_end!(stream) {
        if errors.len() == 0 {
          break Ok(Self(exprs, module));
        } else {
          break Err(ParsingError::Aggregate(errors));
        }
      }

      match Expression::parse(stream, context) {
        Ok(expr) if expr == Expression::default() => (),
        Ok(expr) => exprs.push(expr),
        Err(err) => errors.push(err),
      };

      skip!(stream, Semicolon | NewLine);
    }
  }
}

impl Evaluatable for Script {
  fn evaluate<L: LoggerTrait>(
    &self,
    env: &mut Enviroment,
    logger: &mut L,
  ) -> Result<value::Value, RuntimeError> {
    for expr in self.0.iter() {
      expr.evaluate(env, logger)?;
    }

    Ok(Value::None)
  }
}
