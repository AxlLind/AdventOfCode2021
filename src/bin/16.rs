use std::collections::VecDeque;

static HEX_BITS: [&str; 16] = ["0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"];

enum Inst {
  Literal(usize, usize),
  Operator(usize, u8, Vec<Inst>),
}

fn consume_bits(bits: &mut VecDeque<u8>, n: usize) -> usize {
  let mut x = 0;
  for b in bits.drain(0..n) {
    x = (x<<1) | b as usize;
  }
  x
}

fn parse_inst(b: &mut VecDeque<u8>) -> Inst {
  let version = consume_bits(b,3);
  match consume_bits(b,3) as u8 {
    4 => {
      let (mut val, mut x) = (0,!0);
      while x >> 4 != 0 {
        x = consume_bits(b,5);
        val = (val << 4) + (x & 0xf);
      }
      Inst::Literal(version, val)
    }
    id => {
      let insts = match consume_bits(b,1) {
        0 => {
          let nbits = consume_bits(b,15);
          let mut q = b.drain(0..nbits).collect::<VecDeque<_>>();
          let mut insts = Vec::new();
          while !q.is_empty() {
            insts.push(parse_inst(&mut q));
          }
          insts
        }
        _ => (0..consume_bits(b,11)).map(|_| parse_inst(b)).collect()
      };
      Inst::Operator(version, id, insts)
    }
  }
}

fn version_sum(inst: &Inst) -> usize {
  match inst {
    Inst::Literal(v,_) => *v,
    Inst::Operator(v,_,insts) => *v + insts.iter().map(version_sum).sum::<usize>()
  }
}

fn value(inst: &Inst) -> usize {
  match inst {
    Inst::Literal(_,val) => *val as usize,
    Inst::Operator(_,id,insts) => match id {
      0 => insts.iter().map(value).sum(),
      1 => insts.iter().map(value).product(),
      2 => insts.iter().map(value).min().unwrap(),
      3 => insts.iter().map(value).max().unwrap(),
      5 => (value(&insts[0]) >  value(&insts[1])) as usize,
      6 => (value(&insts[0]) <  value(&insts[1])) as usize,
      7 => (value(&insts[0]) == value(&insts[1])) as usize,
      _ => unreachable!()
    }
  }
}

#[aoc::main(16)]
fn main(input: &str) -> (usize,usize) {
  let mut bits = input.bytes()
    .map(|c| match c {
      b'0'..=b'9' => HEX_BITS[(c - b'0') as usize],
      b'A'..=b'F' => HEX_BITS[(c - b'A') as usize + 10],
      _ => unreachable!(),
    })
    .flat_map(|bits| bits.bytes().map(|b| b - b'0'))
    .collect();
  let inst = parse_inst(&mut bits);
  (version_sum(&inst), value(&inst))
}
