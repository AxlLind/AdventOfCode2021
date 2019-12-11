use std::time::Instant;
use itertools::Itertools;

fn parse_inst(code: i32) -> (i32, bool, bool) {
  let opcode = code % 100;
  let imm1 = (code / 100)  % 10 == 1;
  let imm2 = (code / 1000) % 10 == 1;
  (opcode, imm1, imm2)
}

fn fetch(program: &[i32], pos: usize, imm: bool) -> i32 {
  let index = if imm { pos } else { program[pos] as usize };
  program[index]
}

fn execute(phase_setting: i32, input: i32) -> i32 {
  let mut output = 0;
  let mut has_set_phase = false;
  let mut program = [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99];
  let mut pc = 0;
  loop {
    let (opcode, imm1, imm2) = parse_inst(program[pc]);
    match opcode {
      1 => { // add
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        let c = fetch(&program, pc+3, true) as usize;
        program[c] = a + b;
        pc += 4;
      },
      2 => { // mul
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        let c = fetch(&program, pc+3, true) as usize;
        program[c] = a * b;
        pc += 4;
      },
      3 => { // input
        let a = fetch(&program, pc+1, true) as usize;
        program[a] = if has_set_phase { input } else { phase_setting };
        has_set_phase = true;
        pc += 2;
      },
      4 => { // output
        output = fetch(&program, pc+1, imm1);
        pc += 2;
      },
      5 => { // jnz
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        pc = if a != 0 { b as usize } else { pc + 3 };
      },
      6 => { // jz
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        pc = if a == 0 { b as usize } else { pc + 3 };
      },
      7 => { // slt
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        let c = fetch(&program, pc+3, true) as usize;
        program[c] = (a < b) as i32;
        pc += 4;
      },
      8 => { // seq
        let a = fetch(&program, pc+1, imm1);
        let b = fetch(&program, pc+2, imm2);
        let c = fetch(&program, pc+3, true) as usize;
        program[c] = (a == b) as i32;
        pc += 4;
      },
      _ => break
    }
  }
  output
}

fn main() {
  let now = Instant::now();
  let max = [0,1,2,3,4].iter()
    .permutations(5)
    .map(|digits| digits.iter()
      .fold(0, |prev, i| execute(**i, prev))
    )
    .max()
    .unwrap();
  println!("{:?}", max);
  println!("Time: {}ms", now.elapsed().as_millis());
}
