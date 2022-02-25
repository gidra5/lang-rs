#[macro_export]
macro_rules! read_reg {
  ($thread:ident, $reg:expr) => {
    *$thread.registers.get_unchecked($reg as usize)
  };
}

#[macro_export]
macro_rules! write_reg {
  ($thread:ident, $reg:expr) => {
    *$thread.registers.get_unchecked_mut($reg as usize)
  };
}

#[macro_export]
macro_rules! read_stack {
  ($thread:ident, $reg:expr) => {
    *$thread.stack.get_unchecked($reg as usize)
  };
}

#[macro_export]
macro_rules! write_stack {
  ($thread:ident, $reg:expr) => {
    *$thread.stack.get_unchecked_mut($reg as usize)
  };
}

#[macro_export]
macro_rules! read_stack_top {
  ($thread:ident) => {{
    use crate::{read_stack, read_reg};
    read_stack!($thread, read_reg!($thread, STACK_PTR_REGISTER))
  }};
}

#[macro_export]
macro_rules! write_stack_top {
  ($thread:ident) => {
    write_stack!($thread, read_reg!($thread, STACK_PTR_REGISTER))
  };
}

#[macro_export]
macro_rules! read_mem {
  ($vm:ident, $reg:expr) => {
    *$vm.memory.get_unchecked($reg as usize)
  };
}

#[macro_export]
macro_rules! write_mem {
  ($vm:ident, $reg:expr) => {
    *$vm.memory.get_unchecked_mut($reg as usize)
  };
}

#[macro_export]
macro_rules! read_cmd {
  ($program:ident, $index:expr) => {
    $program.get($index as usize)
  };
}

#[macro_export]
macro_rules! read_const {
  ($constants:ident, $index:expr) => {
    *$constants.get_unchecked($index as usize)
  };
}

#[macro_export]
macro_rules! read_next_cmd {
  ($program:ident, $thread:ident) => {{
    use crate::{read_cmd, read_reg};
    read_cmd!($program, read_reg!($thread, IP_REGISTER))
  }};
}

#[macro_export]
macro_rules! unpack_reg {
  ($packed:expr) => {
    [(($packed >> 4) << 4) as usize, (($packed << 4) >> 4) as usize]
  };
}

#[macro_export]
macro_rules! unpack_reg_val {
  ($thread:ident, $packed:expr) => {
    [read_reg!($thread, (($packed >> 4) << 4) as usize), read_reg!($thread, (($packed << 4) >> 4) as usize)]
  };
}

#[macro_export]
macro_rules! unpack_cmd {
  ($cmd:ident { $($cmd_t:ident $([$reg1:ident $(, $reg2:ident)?])? => $body:tt),* $(,)? }) => {
    match $cmd {
      $(Command::$cmd_t $(($reg1))? => unpack_cmd!($( $([$reg1, $reg2] =>)?)? $body)),*
    }
  };
  ($body:tt) => {
    $body
  };
  ([$reg:ident] => $body:tt) => {
    $body
  };
  ([$reg1:ident, $reg2:ident] => $body:tt) => {
    {
      use crate::unpack_reg;
      let [$reg1, $reg2] = unpack_reg!($reg1);
      $body
    }
  };
}
