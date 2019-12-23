#![allow(dead_code)]
use std::collections::VecDeque;

#[derive(Eq, PartialEq)]
pub enum ExitCode {
  Output(i64),
  AwaitInput,
  Halted
}

#[derive(Clone, Default)]
pub struct IntCoder {
  program: Vec<i64>,
  input: VecDeque<i64>,
  halted: bool,
  rel_base: i64,
  pc: i64,
}

impl IntCoder {
  pub fn new(p: &[i64]) -> Self {
    Self { program: p.into(), ..Self::default() }
  }

  pub fn execute(&mut self) -> ExitCode {
    while !self.halted {
      let (opcode, a, b, c, mode1) = self.fetch_inst();
      match opcode {
        1  => self.set(c, a + b),            // add
        2  => self.set(c, a * b),            // mul
        4  => return ExitCode::Output(a),    // output
        5  => if a != 0 { self.pc = b; },    // jnz
        6  => if a == 0 { self.pc = b; },    // jz
        7  => self.set(c, a < b),            // slt
        8  => self.set(c, a == b),           // seq
        9  => self.rel_base += a,            // rel_base
        99 => self.halted = true,            // halt
        3  => match self.input.pop_front() { // input
          Some(input) => {
            let a = self.fetch_set_adr(1, mode1);
            self.set(a, input);
            self.pc += 2;
          }
          None => return ExitCode::AwaitInput,
        }
        _ => unreachable!("invalid opcode {}", opcode)
      }
    }
    ExitCode::Halted
  }

  pub fn execute_until_output(&mut self) -> i64 {
    match self.execute() {
      ExitCode::Output(o) => o,
      _ => unreachable!("Assumed CPU would exit with output"),
    }
  }

  pub fn push_input<T: Into<i64>>(&mut self, input: T) {
    self.input.push_back(input.into());
  }

  pub fn has_halted(&self) -> bool { self.halted }
}

// private methods
impl IntCoder {
  fn get(&self, adr: i64) -> i64 {
    *self.program.get(adr as usize).unwrap_or(&0)
  }

  fn set<T: Into<i64>>(&mut self, adr: i64, val: T) {
    assert!(adr >= 0, "write to negative address");
    let adr = adr as usize;
    if adr >= self.program.len() {
      self.program.resize(adr + 1, 0);
    }
    self.program[adr] = val.into();
  }

  fn fetch_adr(&self, offset: i64, mode: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match mode {
      0 => self.get(value),
      1 => value,
      2 => self.get(value + self.rel_base),
      _ => unreachable!("invalid mode {}", mode)
    }
  }

  fn fetch_set_adr(&self, offset: i64, mode: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match mode {
      0|1 => value,
      2   => value + self.rel_base,
      _   => unreachable!("invalid mode {}", mode),
    }
  }

  fn fetch_inst(&mut self) -> (i64,i64,i64,i64,i64) {
    let code = self.get(self.pc);
    let mode1 = (code / 100) % 10;
    let mode2 = (code / 1000) % 10;
    let mode3 = (code / 10000) % 10;
    let a = self.fetch_adr(1, mode1);
    let b = self.fetch_adr(2, mode2);
    let c = self.fetch_set_adr(3, mode3);

    let opcode = code % 100;
    self.pc += match opcode {
      1|2|7|8 => 4,
      4|9     => 2,
      5|6     => 3,
      3|99    => 0,
      _ => unreachable!("invalid opcode {}", opcode),
    };
    (opcode, a, b, c, mode1)
  }
}
