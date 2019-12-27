#![allow(dead_code)]
use std::collections::VecDeque;

const ADD: i64 = 1; const MUL: i64 = 2;
const IN:  i64 = 3; const OUT: i64 = 4;
const JNZ: i64 = 5; const JZ:  i64 = 6;
const SLT: i64 = 7; const SEQ: i64 = 8;
const BSE: i64 = 9; const HLT: i64 = 99;

pub enum ExitCode {
  Output(i64),
  AwaitInput,
  Halted
}

#[derive(Clone, Default)]
pub struct IntCoder {
  memory: Vec<i64>,
  input: VecDeque<i64>,
  rel_base: i64,
  pc: i64,
}

impl IntCoder {
  pub fn new(program: &[i64]) -> Self {
    Self { memory: program.into(), ..Self::default() }
  }

  pub fn execute(&mut self) -> ExitCode {
    loop {
      let (opcode, a, b, c, mode1) = self.fetch_inst();
      match opcode {
        ADD => self.set(c, a + b),
        MUL => self.set(c, a * b),
        OUT => return ExitCode::Output(a),
        JNZ => if a != 0 { self.pc = b; },
        JZ  => if a == 0 { self.pc = b; },
        SLT => self.set(c, a < b),
        SEQ => self.set(c, a == b),
        BSE => self.rel_base += a,
        HLT => return ExitCode::Halted,
        IN  => match self.input.pop_front() {
          Some(input) => {
            let a = self.fetch_set_adr(1, mode1);
            self.set(a, input);
            self.pc += 2;
          }
          None => return ExitCode::AwaitInput
        }
        _ => unreachable!("invalid opcode {}", opcode)
      }
    }
  }

  pub fn execute_until_output(&mut self) -> i64 {
    match self.execute() {
      ExitCode::Output(o) => o,
      _ => unreachable!("assumed CPU would exit with output"),
    }
  }

  pub fn push_input<T: Into<i64>>(&mut self, input: T) {
    self.input.push_back(input.into());
  }

  pub fn push_str(&mut self, s: &str) {
    for b in s.bytes() { self.push_input(b); }
  }
}

// private methods
impl IntCoder {
  fn set<T: Into<i64>>(&mut self, adr: i64, val: T) {
    assert!(adr >= 0, "write to negative address");
    let adr = adr as usize;
    if adr >= self.memory.len() {
      self.memory.resize(adr + 1, 0);
    }
    self.memory[adr] = val.into();
  }

  fn get(&self, adr: i64) -> i64 {
    *self.memory.get(adr as usize).unwrap_or(&0)
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
      ADD|MUL|SLT|SEQ => 4,
      OUT|BSE         => 2,
      JNZ|JZ          => 3,
      HLT|IN          => 0,
      _ => unreachable!("invalid opcode {}", opcode),
    };
    (opcode, a, b, c, mode1)
  }
}
