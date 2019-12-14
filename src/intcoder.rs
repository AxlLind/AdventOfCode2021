#![allow(dead_code)]
use std::collections::{HashMap, VecDeque};

#[derive(Eq, PartialEq, Debug)]
pub enum ExitCode {
  Output(i64),
  AwaitInput,
  Halted
}

#[derive(Clone, Default)]
pub struct IntCoder {
  program: HashMap<i64,i64>,
  input: VecDeque<i64>,
  halted: bool,
  ptr_offset: i64,
  pc: i64,
}

impl IntCoder {
  pub fn new(p: &[i64]) -> Self {
    let program = p.iter()
      .enumerate()
      .map(|(k,&v)| (k as i64, v))
      .collect();
    Self { program, ..Self::default() }
  }

  pub fn has_halted(&self) -> bool { self.halted }

  pub fn execute(&mut self) -> ExitCode {
    while !self.halted {
      let (opcode, a, b, c, mode1) = self.fetch_inst();
      match opcode {
        1  => { self.set(c, a + b);         } // add
        2  => { self.set(c, a * b);         } // mul
        4  => { return ExitCode::Output(a); } // output
        5  => { if a != 0 { self.pc = b }   } // jnz
        6  => { if a == 0 { self.pc = b }   } // jz
        7  => { self.set(c, a < b);         } // slt
        8  => { self.set(c, a == b);        } // seq
        9  => { self.ptr_offset += a;       } // ptr_offset
        99 => { self.halted = true;         } // halt
        3  => match self.input.pop_front() {  // input
          Some(input) => {
            let a = self.fetch_set_adr(1, mode1);
            self.set(a, input);
            self.pc += 2;
          }
          None => { return ExitCode::AwaitInput; }
        }
        _ => unreachable!("invalid opcode {}", opcode)
      }
    }
    ExitCode::Halted
  }

  pub fn execute_until_output(&mut self) -> i64 {
    match self.execute() {
      ExitCode::Output(o) => o,
      _ => unreachable!(),
    }
  }

  pub fn push_input(&mut self, input: i64) {
    self.input.push_back(input);
  }
}

// private methods
impl IntCoder {
  fn get(&self, adr: i64) -> i64 {
    *self.program.get(&adr).unwrap_or(&0)
  }

  fn set<T>(&mut self, adr: i64, val: T) where i64: From<T> {
    assert!(adr >= 0, "write to negative address");
    self.program.insert(adr, val.into());
  }

  fn inst_len(&self, opcode: i64) -> i64 {
    match opcode {
      1|2|7|8 => 4,
      4|9     => 2,
      5|6     => 3,
      3|99    => 0,
      _ => unreachable!("invalid opcode {}", opcode),
    }
  }

  fn fetch_adr(&self, offset: i64, mode: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match mode {
      0 => self.get(value),
      1 => value,
      2 => self.get(value + self.ptr_offset),
      _ => unreachable!("invalid mode {}", mode)
    }
  }

  fn fetch_set_adr(&self, offset: i64, mode: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match mode {
      0|1 => value,
      2   => value + self.ptr_offset,
      _   => unreachable!("invalid mode {}", mode),
    }
  }

  fn fetch_inst(&mut self) -> (i64,i64,i64,i64,i64) {
    let code = self.get(self.pc);
    let opcode = code % 100;
    let mode1 = (code / 100) % 10;
    let mode2 = (code / 1000) % 10;
    let mode3 = (code / 10000) % 10;
    let a = self.fetch_adr(1, mode1);
    let b = self.fetch_adr(2, mode2);
    let c = self.fetch_set_adr(3, mode3);
    self.pc += self.inst_len(opcode);
    (opcode, a, b, c, mode1)
  }
}
