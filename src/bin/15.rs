use std::collections::*;

fn solve(target: usize) -> usize {
  let mut seen = [9,19,1,6,0,5].iter()
    .enumerate()
    .map(|(i,&e)| (e, i+1))
    .collect::<HashMap<_,_>>();
  (7..target).fold(4, |last, i| i - seen.insert(last, i).unwrap_or(i))
}

aoc2020::main! {
  (solve(2020), solve(30_000_000))
}
