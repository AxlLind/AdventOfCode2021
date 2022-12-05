use itertools::Itertools;

fn part_one(instructions: &[(usize, usize, usize)], mut stacks: Vec<Vec<char>>) -> String {
  for &(times, from, to) in instructions {
    for _ in 0..times {
      let item = stacks[from-1].pop().unwrap();
      stacks[to-1].push(item);
    }
  }
  stacks.iter().map(|s| s.last().unwrap()).join("")
}

fn part_two(instructions: &[(usize, usize, usize)], mut stacks: Vec<Vec<char>>) -> String {
  for &(times, from, to) in instructions {
    let len = stacks[to-1].len() + times;
    stacks[to-1].resize(len, 'x');
    for i in 0..times {
      let item = stacks[from-1].pop().unwrap();
      stacks[to-1][len-1-i] = item;
    }
  }
  stacks.iter().map(|s| s.last().unwrap()).join("")
}

#[aoc::main(05)]
fn main(input: &str) -> (String, String) {
  let (boxes, rest) = input.split_once("\n\n").unwrap();
  let mut stacks = vec![vec![]; 9];
  for l in boxes.lines().rev().map(str::as_bytes).filter(|l| l[0] == b'[') {
    for i in 0..stacks.len() {
      let c = l[i*4+1];
      if c.is_ascii_alphabetic() {
        stacks[i].push(c as char);
      }
    }
  }
  let instructions = rest.lines()
    .map(|l| l.split_whitespace()
      .filter_map(|s| s.parse::<usize>().ok())
      .collect_tuple()
      .unwrap()
    )
    .collect::<Vec<_>>();
  let p1 = part_one(&instructions, stacks.clone());
  let p2 = part_two(&instructions, stacks);
  (p1,p2)
}
