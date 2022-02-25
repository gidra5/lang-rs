#![allow(unused)]

use std::sync::{Mutex, RwLock};

use itertools::Itertools;

use crate::{
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

const STACK_SIZE: usize = 256;
const REGISTERS_COUNT: usize = 16;
const THREADS_COUNT: usize = 1;
const IP_REGISTER: usize = 15;
const STACK_PTR_REGISTER: usize = 14;
const STACK_BASE_PTR_REGISTER: usize = 13;
const MAIN_THREAD: usize = 0;
const MEMORY_SIZE: usize = 256 * 256;
type RegisterSize = u16;
type RegisterIndex = u8;
/// contains two 4-bit indicies
type PackedRegisterIndex = u8;

#[derive(Clone, Copy)]
pub enum Command {
  Stop,
  /// read constant
  Const(usize),
  /// `reg1 - stack <- reg1`
  Push(RegisterIndex),
  /// `reg1 - stack -> reg1`
  Pop(RegisterIndex),
  /// `reg1 - reg1 = reg1 << 1`
  ShiftLeft(RegisterIndex),
  /// `reg1 - reg1 = reg1 >> 1`
  ShiftRight(RegisterIndex),
  /// `reg1, reg2 - stack[reg2] -> reg1`
  ReadStack(PackedRegisterIndex),
  /// `reg1, reg2 - stack[reg2] <- reg1`
  WriteStack(PackedRegisterIndex),
  /// `reg1, reg2 - reg1 = ~(reg1 & reg2)`
  NotAnd(PackedRegisterIndex),
  /// `reg1, reg2 - reg1 = reg1 + reg2
  Add(PackedRegisterIndex),
  /// `reg1, reg2 - reg1 = reg1 * reg2
  Mult(PackedRegisterIndex),
  /// `reg, [mem] - reg = [mem]`
  Load(PackedRegisterIndex),
  /// `[mem], reg - [mem] = reg`
  Store(PackedRegisterIndex),
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
        Const[index] => {
          write_stack_top!(self) = read_const!(constants, index);
          write_reg!(self, STACK_PTR_REGISTER) = read_reg!(self, STACK_PTR_REGISTER) + 1
        },
        Push[reg] => {
          write_stack_top!(self) = read_reg!(self, reg);
          write_reg!(self, STACK_PTR_REGISTER) = read_reg!(self, STACK_PTR_REGISTER) + 1
        },
        Pop[reg] => {
          write_reg!(self, STACK_PTR_REGISTER) = read_reg!(self, STACK_PTR_REGISTER) - 1;
          write_reg!(self, reg) = read_stack_top!(self)
        },
        ShiftLeft[reg] => { write_reg!(self, reg) = read_reg!(self, reg) << 1 },
        ShiftRight[reg] => { write_reg!(self, reg) = read_reg!(self, reg) >> 1 },
        ReadStack[reg1, reg2] => { write_reg!(self, reg1) = read_stack!(self,read_reg!(self, reg2)) },
        WriteStack[reg1, reg2] => { write_stack!(self, read_reg!(self, reg2)) = read_reg!(self, reg1) },
        NotAnd[reg1, reg2] => { write_reg!(self, reg1) = !(read_reg!(self, reg1) & read_reg!(self, reg2)) },
        Add[reg1, reg2] => { write_reg!(self, reg1) = read_reg!(self, reg1) + read_reg!(self, reg2) },
        Mult[reg1, reg2] => { write_reg!(self, reg1) = read_reg!(self, reg1) * read_reg!(self, reg2) },
        Load[reg1, reg2] => { write_reg!(self, reg1) = read_mem!(vm, read_reg!(self, reg2)) },
        Store[reg1, reg2] => { write_mem!(vm, read_reg!(self, reg1)) = read_reg!(self, reg2) },
      });
    }
  }
  pub fn execute_sync(&mut self, vm: &mut SyncVM) {}
  pub fn stack_empty(&self) -> bool { self.registers[STACK_PTR_REGISTER] == 0 }
}

pub struct SyncVM {
  memory: [RwLock<RegisterSize>; MEMORY_SIZE],
}

pub struct VM {
  memory: [RegisterSize; MEMORY_SIZE],
}

impl VM {
  pub fn new() -> VM {
    VM {
      memory: [0; MEMORY_SIZE],
    }
  }
  // pub unsafe fn sync(self) -> SyncVM {}
  pub unsafe fn execute(&mut self, program: &[Command], constants: &[RegisterSize]) {
    let mut threads = [Thread::new(); THREADS_COUNT];
    self.execute_on_threads(threads, program, constants)
  }
  pub unsafe fn execute_on_threads(
    &mut self,
    mut threads: [Thread; THREADS_COUNT],
    program: &[Command],
    constants: &[RegisterSize],
  ) {
    threads[MAIN_THREAD].execute(self, program, constants)
  }
}
