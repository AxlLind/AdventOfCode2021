use std::time::Instant;
use itertools::Itertools;

fn to_digits(n: usize) -> [usize; 6] {
  [
    (n / 100000) % 10,
    (n / 10000) % 10,
    (n / 1000) % 10,
    (n / 100) % 10,
    (n / 10) % 10,
    (n / 1) % 10,
  ]
}

fn part_one() -> usize {
  (206938..679128)
    .map(to_digits)
    .filter(|&digits| (1..digits.len()).all(|i| digits[i-1] <= digits[i]))
    .filter(|&digits| (1..digits.len()).any(|i| digits[i-1] == digits[i]))
    .count()
}

fn part_two() -> usize {
  (206938..679128)
    .map(to_digits)
    .filter(|&digits| (1..digits.len()).all(|i| digits[i-1] <= digits[i]))
    .filter(|&digits| digits.iter()
      .group_by(|d| *d)
      .into_iter()
      .any(|(_,x)| x.count() == 2)
    )
    .count()
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
