use itertools::Itertools;

#[aoc::main(09)]
fn main(input: &str) -> (isize, isize) {
  input.split('\n').map(|l| {
    let xs = l.split_whitespace()
      .map(|w| w.parse::<isize>().unwrap())
      .collect::<Vec<_>>();
    let mut v = vec![xs];
    while v[v.len()-1].iter().any(|&x| x != 0) {
      let xs = v[v.len()-1].iter()
        .tuple_windows()
        .map(|(a,b)| b - a)
        .collect();
      v.push(xs);
    }
    v.iter().rev().fold((0,0), |(p1, p2), xs| (xs[xs.len()-1] + p1, xs[0] - p2))
  }).fold((0, 0), |(a,b),(x,y)| (a+x, b+y))
}
