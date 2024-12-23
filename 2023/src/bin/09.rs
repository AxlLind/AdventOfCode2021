use itertools::Itertools;

#[aoc::main(09)]
fn main(input: &str) -> (isize, isize) {
  let (mut p1, mut p2) = (0,0);
  for l in input.split('\n') {
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
    let mut b = 0;
    for xs in v.iter().rev() {
      p1 += xs[xs.len()-1];
      b = xs[0] - b;
    }
    p2 += b;
  }
  (p1, p2)
}
