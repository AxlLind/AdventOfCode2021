use hashbrown::HashSet;

static INPUT: &str = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 16\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 14\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 0\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -8\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 1\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -16\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -4\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -3\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 12\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y";

#[derive(Clone, Copy)]
enum Op { Add, Mul, Div, Mod, Eql }

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

fn find_modelnum(visited: &mut HashSet<(i64, usize)>, blocks: &[Vec<(Op,usize,Source)>], block: usize, z: i64, range: &[i64;9]) -> Option<i64> {
  if block == blocks.len() {
    return if z == 0 {Some(0)} else {None};
  }
  if visited.contains(&(z,block)) { return None; }

  for &digit in range {
    let mut regs = [digit,0,0,z];
    for &inst in &blocks[block] {
      match inst {
        (Op::Add, a,b) => regs[a] += b.val(&regs),
        (Op::Mul, a,b) => regs[a] *= b.val(&regs),
        (Op::Div, a,b) => regs[a] /= b.val(&regs),
        (Op::Mod, a,b) => regs[a] %= b.val(&regs),
        (Op::Eql, a,b) => regs[a] = (regs[a] == b.val(&regs)) as i64,
      }
    }
    if let Some(best) = find_modelnum(visited, blocks, block+1, regs[3], range) {
      return Some(best*10 + digit)
    }
  }
  visited.insert((z,block));
  None
}

fn solve(blocks: &[Vec<(Op,usize,Source)>], biggest: bool) -> String {
  let range = if biggest {[9,8,7,6,5,4,3,2,1]} else {[1,2,3,4,5,6,7,8,9]};
  let answer = find_modelnum(&mut HashSet::new(), &blocks, 0, 0, &range).unwrap();
  answer.to_string().chars().rev().collect()
}

aoc2021::main! {
  let insts = INPUT.lines()
    .filter(|l| !l.starts_with("inp"))
    .map(|l| {
      let op =  match &l[..3] {
        "add" => Op::Add,
        "mul" => Op::Mul,
        "div" => Op::Div,
        "mod" => Op::Mod,
        "eql" => Op::Eql,
        _ => unreachable!()
      };
      let src = match Source::from_str(&l[4..5]) {
        Source::Reg(src) => src,
        _ => unreachable!()
      };
      let dest = Source::from_str(&l[6..]);
      (op,src,dest)
    })
    .collect::<Vec<_>>();
  let blocks = insts.chunks(17).map(|c| c.iter().copied().collect()).collect::<Vec<_>>();
  (solve(&blocks, true), solve(&blocks, false))
}
