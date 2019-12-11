use std::time::Instant;
use std::cmp;
use std::collections::{HashMap, VecDeque};

static PROGRAM: [i64; 606] = [3,8,1005,8,284,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,72,1006,0,24,1,1106,12,10,1006,0,96,1,1008,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,108,1006,0,54,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,134,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,60,1006,0,64,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,183,1006,0,6,1006,0,62,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,211,1,108,0,10,2,1002,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,242,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,263,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,606,104,0,104,1,21101,0,666526126996,1,21101,301,0,0,1105,1,405,21101,846138811028,0,1,21101,312,0,0,1106,0,405,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,248129978391,1,21101,359,0,0,1105,1,405,21101,97751403560,0,1,21102,1,370,0,1106,0,405,3,10,104,0,104,0,3,10,104,0,104,0,21101,988753585000,0,1,21101,393,0,0,1105,1,405,21102,867961709324,1,1,21102,404,1,0,1106,0,405,99,109,2,22102,1,-1,1,21102,40,1,2,21101,436,0,3,21102,1,426,0,1105,1,469,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,431,432,447,4,0,1001,431,1,431,108,4,431,10,1006,10,463,1102,0,1,431,109,-2,2106,0,0,0,109,4,1202,-1,1,468,1207,-3,0,10,1006,10,486,21102,1,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,505,0,0,1106,0,510,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,533,2207,-4,-2,10,1006,10,533,22101,0,-4,-4,1105,1,601,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,552,0,1105,1,510,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,571,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,593,21202,-1,1,1,21102,1,593,0,106,0,468,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0];

#[derive(Eq, PartialEq, Debug)]
enum ExitCode {
  Output(i64),
  AwaitInput,
  Halted
}

#[derive(Clone, Default)]
struct IntCoder {
  program: HashMap<i64,i64>,
  input: VecDeque<i64>,
  halted: bool,
  ptr_offset: i64,
  pc: i64,
}

impl IntCoder {
  fn new(p: &[i64]) -> Self {
    let program = p.iter()
      .enumerate()
      .map(|(k,&v)| (k as i64, v))
      .collect();
    Self { program, ..Self::default() }
  }

  fn get(&self, adr: i64) -> i64 {
    assert!(adr >= 0, "read from negative address");
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

  fn execute(&mut self) -> ExitCode {
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

  fn push_input(&mut self, input: i64) {
    self.input.push_back(input);
  }
}

fn turn(curr: char, dir: i64) -> char {
  match curr {
    'U' => if dir == 0 {'L'} else {'R'},
    'D' => if dir == 0 {'R'} else {'L'},
    'L' => if dir == 0 {'D'} else {'U'},
    'R' => if dir == 0 {'U'} else {'D'},
    _   => unreachable!()
  }
}

fn print_map(map: &HashMap<(i64,i64),i64>) {
  let (xmin,ymin,xmax,ymax) = map.keys()
    .fold((0,0,0,0), |(xmin,ymin,xmax,ymax), &(x,y)| {
      let xmin = cmp::min(x, xmin);
      let ymin = cmp::min(y, ymin);
      let xmax = cmp::max(x, xmax);
      let ymax = cmp::max(y, ymax);
      (xmin,ymin,xmax,ymax)
    });
  for i in xmin..=xmax {
    for j in ymin..=ymax {
      let val = *map.get(&(i,j)).unwrap_or(&0);
      print!("{}", if val == 0 {' '} else {'■'});
    }
    println!("");
  }
}

fn main() {
  let now = Instant::now();
  let mut cpu = IntCoder::new(&PROGRAM);
  let mut map = HashMap::new();
  map.insert((0,0), 1); // comment this out for part 1

  let mut x = 0;
  let mut y = 0;
  let mut dir = 'U';
  loop {
    match cpu.execute() {
      ExitCode::Output(c) => match cpu.execute() {
        ExitCode::Output(i) => {
          map.insert((x,y), c);
          dir = turn(dir, i);
          match dir {
            'U' => x -= 1,
            'D' => x += 1,
            'L' => y -= 1,
            'R' => y += 1,
            _   => unreachable!()
          }
        }
        _ => unreachable!()
      }
      ExitCode::AwaitInput => cpu.push_input(*map.get(&(x,y)).unwrap_or(&0)),
      ExitCode::Halted => break,
    }
  }
  print_map(&map);
  println!("Squares: {}", map.len());
  println!("Time: {}ms", now.elapsed().as_millis());
}
