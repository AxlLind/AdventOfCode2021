#![allow(dead_code)]
use std::collections::VecDeque;

const ADD: i64 = 1; const MUL: i64 = 2;
const IN:  i64 = 3; const OUT: i64 = 4;
const JNZ: i64 = 5; const JZ:  i64 = 6;
const SLT: i64 = 7; const SEQ: i64 = 8;
const BSE: i64 = 9; const HLT: i64 = 99;

pub enum ExitCode { Output(i64), AwaitInput, Halted }

#[derive(Clone)]
pub struct IntCoder {
  ram: Vec<i64>,
  input: VecDeque<i64>,
  rel_base: i64,
  pc: i64,
}

impl IntCoder {
  pub fn new(program: &[i64]) -> Self {
    Self {
      ram: program.into(),
      input: VecDeque::new(),
      rel_base: 0,
      pc: 0
    }
  }

  pub fn execute(&mut self) -> ExitCode {
    loop {
      let (op,a,b,c) = self.fetch_inst();
      match op {
        ADD => self.set(c, a + b),
        MUL => self.set(c, a * b),
        SLT => self.set(c, a < b),
        SEQ => self.set(c, a == b),
        BSE => self.rel_base += a,
        JNZ => if a != 0 { self.pc = b },
        JZ  => if a == 0 { self.pc = b },
        OUT => return ExitCode::Output(a),
        HLT => return ExitCode::Halted,
        IN  => match self.input.pop_front() {
          Some(i) => {
            let a = self.fetch_write_adr(1);
            self.set(a,i);
            self.pc += 2;
          }
          None => return ExitCode::AwaitInput
        }
        _ => panic!("invalid opcode {}", op)
      }
    }
  }

  pub fn execute_until_output(&mut self) -> i64 {
    match self.execute() {
      ExitCode::Output(o) => o,
      _ => panic!("assumed CPU would exit with output"),
    }
  }

  pub fn push_input<T: Into<i64>>(&mut self, input: T) {
    self.input.push_back(input.into());
  }

  pub fn push_str(&mut self, s: &str) {
    for b in s.bytes() { self.push_input(b); }
    self.push_input(b'\n');
  }
}

// private methods
impl IntCoder {
  fn set<T: Into<i64>>(&mut self, adr: i64, val: T) {
    assert!(adr >= 0, "write to negative address");
    let adr = adr as usize;
    if adr >= self.ram.len() { self.ram.resize(adr + 1, 0); }
    self.ram[adr] = val.into();
  }

  fn get(&self, adr: i64) -> i64 {
    *self.ram.get(adr as usize).unwrap_or(&0)
  }

  fn get_mode(&self, offset: i64) -> i64 {
    let d = match offset {
      1 => 100,
      2 => 1000,
      3 => 10000,
      _ => unreachable!(),
    };
    self.get(self.pc) / d % 10
  }

  fn fetch_adr(&self, offset: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match self.get_mode(offset) {
      0 => self.get(value),
      1 => value,
      2 => self.get(value + self.rel_base),
      _ => unreachable!("invalid mode")
    }
  }

  fn fetch_write_adr(&self, offset: i64) -> i64 {
    let value = self.get(self.pc + offset);
    match self.get_mode(offset) {
      0|1 => value,
      2   => value + self.rel_base,
      _   => unreachable!("invalid mode"),
    }
  }

  fn fetch_inst(&mut self) -> (i64,i64,i64,i64) {
    let a = self.fetch_adr(1);
    let b = self.fetch_adr(2);
    let c = self.fetch_write_adr(3);
    let op = self.get(self.pc) % 100;
    self.pc += match op {
      ADD|MUL|SLT|SEQ => 4,
      OUT|BSE         => 2,
      JNZ|JZ          => 3,
      _               => 0,
    };
    (op,a,b,c)
  }
}
