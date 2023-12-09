use itertools::Itertools;

fn solve(xs: Vec<isize>) -> (isize, isize) {
  let mut v = vec![xs];
  while v[v.len()-1].iter().any(|&x| x != 0) {
    let xs = v[v.len()-1].iter()
      .tuple_windows()
      .map(|(a,b)| b - a)
      .collect();
    v.push(xs);
  }
  v.iter().rev().fold((0,0), |(a,b), xs| (xs[xs.len()-1] + a, xs[0] - b))
}

#[aoc::main(09)]
fn main(input: &str) -> (isize, isize) {
  input.split('\n').fold((0,0), |(p1,p2), l| {
    let xs = l.split_whitespace()
      .map(|w| w.parse::<isize>().unwrap())
      .collect::<Vec<_>>();
    let (a, b) = solve(xs);
    (p1 + a, p2 + b)
  })
}
