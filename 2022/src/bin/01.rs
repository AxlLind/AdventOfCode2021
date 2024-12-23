use itertools::Itertools;

#[aoc::main(01)]
fn main(input: &str) -> (usize, usize) {
  let xs = input.split("\n\n")
    .map(|s| s.lines().map(|l| l.parse::<usize>().unwrap()).sum::<usize>())
    .sorted()
    .rev()
    .collect::<Vec<_>>();
  let p1 = xs[0];
  let p2 = xs[0..3].iter().sum();
  (p1,p2)
}
