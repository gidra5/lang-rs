#![allow(unused)]
pub enum LangVMCommand {
  /// reg1, reg2 - reg1 = ~(reg1 & reg2)
  NotAnd,
  /// reg, [mem] - reg = [mem]
  LoadWord,
  /// [mem], reg - [mem] = reg
  StoreWord,
  /// read next word as extended command
  Extended,
}

pub struct LangVM {}

pub trait VM {
  type Command;
}

impl VM for LangVM {
  type Command = LangVMCommand;
}
