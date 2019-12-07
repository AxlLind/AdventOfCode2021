use itertools::Itertools;

#[derive(Eq, PartialEq, Debug)]
enum ExitCode {
  Output(i32),
  AwaitInput,
  Halted
}

#[derive(Clone)]
struct IntCoder {
  program: Vec<i32>,
  has_halted: bool,
  pc: usize,
}

impl IntCoder {
  fn new(program: &[i32]) -> Self {
    Self {
      program: program.to_vec(),
      has_halted: false,
      pc: 0
    }
  }

  fn parse_inst(&self) -> (i32, bool, bool) {
    let code = self.program[self.pc];
    let opcode = code % 100;
    let imm1 = (code / 100)  % 10 == 1;
    let imm2 = (code / 1000) % 10 == 1;
    (opcode, imm1, imm2)
  }

  fn fetch(&self, offset: usize, imm: bool) -> i32 {
    let pos = self.pc + offset;
    let index = if imm { pos } else { self.program[pos] as usize };
    self.program[index]
  }

  fn execute(&mut self, input: i32) -> ExitCode {
    let mut has_used_input = false;
    loop {
      let (opcode, imm1, imm2) = self.parse_inst();
      match opcode {
        1 => { // add
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          let c = self.fetch(3, true) as usize;
          self.program[c] = a + b;
          self.pc += 4;
        },
        2 => { // mul
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          let c = self.fetch(3, true) as usize;
          self.program[c] = a * b;
          self.pc += 4;
        },
        3 => { // input
          if has_used_input {
            return ExitCode::AwaitInput;
          }
          has_used_input = true;

          let a = self.fetch(1, true) as usize;
          self.program[a] = input;
          self.pc += 2;
        },
        4 => { // output
          let output = self.fetch(1, imm1);
          self.pc += 2;
          return ExitCode::Output(output);
        },
        5 => { // jnz
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          self.pc = if a != 0 { b as usize } else { self.pc + 3 };
        },
        6 => { // jz
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          self.pc = if a == 0 { b as usize } else { self.pc + 3 };
        },
        7 => { // slt
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          let c = self.fetch(3, true) as usize;
          self.program[c] = (a < b) as i32;
          self.pc += 4;
        },
        8 => { // seq
          let a = self.fetch(1, imm1);
          let b = self.fetch(2, imm2);
          let c = self.fetch(3, true) as usize;
          self.program[c] = (a == b) as i32;
          self.pc += 4;
        },
        99 => {
          self.has_halted = true;
          return ExitCode::Halted;
        }
        _ => unreachable!(),
      }
    }
  }
}

fn main() {
  let program = [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99];
  let max = [5,6,7,8,9].iter()
    .permutations(5)
    .map(|digits| {
      let mut coders = vec![IntCoder::new(&program); 5];
      for i in 0..5 { coders[i].execute(*digits[i]); }

      let mut input = 0;
      while !coders[4].has_halted {
        for c in &mut coders {
          if let ExitCode::Output(tmp) = c.execute(input) {
            input = tmp;
          }
        }
      }
      input
    })
    .max();
  println!("{}", max.unwrap());
}
