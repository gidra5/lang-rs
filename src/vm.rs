#![allow(unused)]

use std::sync::{Mutex, RwLock};

use itertools::Itertools;

use crate::{
  pop_stack,
  push_stack,
  read_const,
  read_mem,
  read_next_cmd,
  read_reg,
  read_stack,
  read_stack_top,
  unpack_cmd,
  write_mem,
  write_reg,
  write_stack,
  write_stack_top,
};

#[path = "tests/vm.rs"]
mod tests;

mod utils;

/// inner vm thread stack size
const STACK_SIZE: usize = 256;
const REGISTERS_COUNT: usize = 16;
/// last three registers are used by vm itself
const IP_REGISTER: usize = REGISTERS_COUNT - 1;
const STACK_PTR_REGISTER: usize = REGISTERS_COUNT - 2;
const STACK_FRAME_PTR_REGISTER: usize = REGISTERS_COUNT - 3;
const MAIN_THREAD: usize = 0;
/// aka word size
type RegisterSize = u16;
type RegisterIndex = u8;
/// contains two 4-bit indicies
type PackedRegisterIndex = u8;

#[derive(Clone, Copy)]
pub enum Command {
  /// `i - stack <- constants[i]`
  Const(usize),
  /// `reg1 - stack <- reg1`
  Push(RegisterIndex),
  /// `reg1 - stack -> reg1`
  Pop(RegisterIndex),
  /// `reg1, reg2 - stack[reg2] -> reg1`
  ReadStack(PackedRegisterIndex),
  /// `reg1, reg2 - stack[reg2] <- reg1`
  WriteStack(PackedRegisterIndex),
  /// `stack[top] = stack[top] << 1`
  ShiftLeft,
  /// `stack[top] = stack[top] >> 1`
  ShiftRight,
  /// `stack[top - 1] = ~(stack[top] & stack[top - 1])`
  NotAnd,
  /// `stack[top - 1] = stack[top] + stack[top - 1]
  Add,
  /// `reg1, reg2 - stack[top - 1] = stack[top] * stack[top - 1]
  Mult,
  /// `stack <- [stack[top]]`
  Load,
  /// `[stack[top]] = stack[top - 1]`
  Store,
  /// Finish execution
  Stop,
}

#[derive(Clone, Copy)]
pub struct Thread {
  stack:     [RegisterSize; STACK_SIZE],
  registers: [RegisterSize; REGISTERS_COUNT],
}

impl Thread {
  pub fn new() -> Thread {
    let mut registers = [0; REGISTERS_COUNT];
    Thread {
      stack: [0; STACK_SIZE],
      registers,
    }
  }
  pub unsafe fn execute(&mut self, vm: &mut VM, program: &[Command], constants: &[RegisterSize]) {
    while let Some(&command) = read_next_cmd!(program, self) {
      unpack_cmd!(command {
        Stop => break,
        Const[index] => { push_stack!(self, read_const!(constants, index)); },
        Push[reg] => { push_stack!(self, read_reg!(self, reg)); },
        Pop[reg] => { write_reg!(self, reg) = pop_stack!(self); },
        ShiftLeft => { write_stack_top!(self) = read_stack_top!(self) << 1 },
        ShiftRight => { write_stack_top!(self) = read_stack_top!(self) >> 1 },
        ReadStack[reg1, reg2] => { write_reg!(self, reg1) = read_stack!(self, read_reg!(self, reg2)) },
        WriteStack[reg1, reg2] => { write_stack!(self, read_reg!(self, reg2)) = read_reg!(self, reg1) },
        NotAnd => { push_stack!(self, !(pop_stack!(self) & pop_stack!(self))); },
        Add => { push_stack!(self, pop_stack!(self) + pop_stack!(self)); },
        Mult => { push_stack!(self, pop_stack!(self) * pop_stack!(self)); },
        Load => { push_stack!(self, read_mem!(vm, read_stack_top!(self))); },
        Store => { write_mem!(vm, pop_stack!(self)) = pop_stack!(self); },
      });
    }
  }
  pub fn execute_sync(&mut self, vm: &mut SyncVM) {}
  pub fn stack_empty(&self) -> bool { self.registers[STACK_PTR_REGISTER] == 0 }
}

pub struct SyncVM {
  memory: [RwLock<RegisterSize>],
}

pub struct VM {
  memory: [RegisterSize],
}

impl VM {
  // pub unsafe fn sync(self) -> SyncVM<'a> {}
  pub unsafe fn execute_on_threads(
    &mut self,
    mut threads: &mut [Thread],
    program: &[Command],
    constants: &[RegisterSize],
  ) {
    threads[MAIN_THREAD].execute(self, program, constants)
  }
}
