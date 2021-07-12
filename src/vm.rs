#![allow(unused)]
pub enum LangVMCommand {
  NotAnd,    // reg1, reg2 - reg1 = ~(reg1 & reg2)
  LoadWord,  // reg, [mem] - reg = [mem]
  StoreWord, // [mem], reg - [mem] = reg
  Extended,  // read next word as extended command
}

pub struct LangVM {}

pub trait VM {
  type Command;
}

impl VM for LangVM {
  type Command = LangVMCommand;
}
