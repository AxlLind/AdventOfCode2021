use std::time::Instant;
use itertools::Itertools;

fn part_one() -> usize {
  (206938..679128).filter(|n| {
    let digits = n.to_string()
      .chars()
      .map(|c| c as u8 - b'0')
      .collect::<Vec<_>>();
    let mut repeating = false;
    for i in 1..digits.len() {
      if digits[i] < digits[i-1] { return false; }
      if digits[i] == digits[i-1] { repeating = true; }
    }
    repeating
  }).count()
}

fn part_two() -> usize {
  (206938..679128).filter(|n| {
    let digits = n.to_string()
      .chars()
      .map(|c| c as u8 - b'0')
      .collect::<Vec<_>>();
    if (1..digits.len()).any(|i| digits[i] < digits[i-1]) {
      return false;
    }
    digits.iter()
      .group_by(|d| *d)
      .into_iter()
      .any(|(_,x)| x.count() == 2)
  }).count()
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
