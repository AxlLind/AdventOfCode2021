use hashbrown::HashMap;

static INPUT: &str = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 16\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 14\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 0\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -8\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 1\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -16\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -4\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -3\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 12\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y";

type Cache = HashMap<(i64, usize), Option<i64>>;

#[derive(Clone, Copy)]
enum Source { Reg(usize), Val(i64) }

impl Source {
  fn from_str(s: &str) -> Self {
    match s {
      "w" => Self::Reg(0),
      "x" => Self::Reg(1),
      "y" => Self::Reg(2),
      "z" => Self::Reg(3),
      _   => Self::Val(s.parse().unwrap()),
    }
  }

  fn val(&self, regs: &[i64; 4]) -> i64 {
    match *self {
      Self::Reg(i) => regs[i],
      Self::Val(v) => v,
    }
  }
}

#[derive(Clone, Copy)]
enum Instruction {
  Inp(usize),
  Add(usize, Source),
  Mul(usize, Source),
  Div(usize, Source),
  Mod(usize, Source),
  Eql(usize, Source),
}

fn find_modelnum(memo: &mut Cache, blocks: &[Vec<Instruction>], block: usize, z: i64, range: &[i64;9]) -> Option<i64> {
  if block == blocks.len() {
    return if z == 0 {Some(0)} else {None};
  }
  if let Some(&answer) = memo.get(&(z,block)) { return answer; }

  for &digit in range {
    let mut regs = [digit,0,0,z];
    for &inst in &blocks[block] {
      match inst {
        Instruction::Add(a,b) => regs[a] += b.val(&regs),
        Instruction::Mul(a,b) => regs[a] *= b.val(&regs),
        Instruction::Div(a,b) => regs[a] /= b.val(&regs),
        Instruction::Mod(a,b) => regs[a] %= b.val(&regs),
        Instruction::Eql(a,b) => regs[a] = (regs[a] == b.val(&regs)) as i64,
        Instruction::Inp(_) => unreachable!()
      }
    }
    let z = regs[3];
    if let Some(best) = find_modelnum(memo, blocks, block+1, z, range) {
      memo.insert((z,block), Some(best*10 + digit));
      return Some(best*10 + digit)
    }
  }

  memo.insert((z,block),None);
  None
}

fn solve(blocks: &[Vec<Instruction>], biggest: bool) -> String {
  let range = if biggest {[9,8,7,6,5,4,3,2,1]} else {[1,2,3,4,5,6,7,8,9]};
  let answer = find_modelnum(&mut Cache::new(), &blocks, 0, 0, &range).unwrap();
  answer.to_string().chars().rev().collect()
}

aoc2021::main! {
  let insts = INPUT.lines()
    .map(|l| {
      let src = match Source::from_str(&l[4..5]) {
        Source::Reg(src) => src,
        _ => unreachable!()
      };
      match &l[..3] {
        "inp" => Instruction::Inp(src),
        "add" => Instruction::Add(src, Source::from_str(&l[6..])),
        "mul" => Instruction::Mul(src, Source::from_str(&l[6..])),
        "div" => Instruction::Div(src, Source::from_str(&l[6..])),
        "mod" => Instruction::Mod(src, Source::from_str(&l[6..])),
        "eql" => Instruction::Eql(src, Source::from_str(&l[6..])),
        _ => unreachable!()
      }
    })
    .collect::<Vec<_>>();
  let blocks = insts.chunks(18).map(|c| c.iter().skip(1).copied().collect()).collect::<Vec<_>>();
  (solve(&blocks, true), solve(&blocks, false))
}
