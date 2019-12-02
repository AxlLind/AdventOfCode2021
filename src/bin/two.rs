use itertools::Itertools;

fn fetch_params(program: &[usize], index: usize) -> (usize, usize, usize) {
  let a = program[program[index+1]];
  let b = program[program[index+2]];
  let pos = program[index+3];
  (a,b,pos)
}

fn main() {
  for (i,j) in (0..100).cartesian_product(0..100) {
    let mut program = [1,i,j,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0];
    let mut index = 0;
    loop {
      let opcode = program[index];
      match opcode {
        1|2 => {
          let (a,b,pos) = fetch_params(&program, index);
          program[pos] = if opcode == 1 { a+b } else { a*b };
          index += 4;
        },
        _ => break
      }
    }
    if program[0] == 19690720 {
      return println!("Answer: {} {} {}", i, j, 100 * i + j);
    }
  }
}
