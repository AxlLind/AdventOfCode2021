use std::time::Instant;
use std::ops::Range;
use itertools::Itertools;

const INPUT: Range<usize> = 206938..679128;

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

fn count_passwords() -> (usize,usize) {
  let passwords = INPUT.map(to_digits)
    .filter(|d| (1..d.len()).all(|i| d[i-1] <= d[i]))
    .filter(|d| (1..d.len()).any(|i| d[i-1] == d[i]))
    .map(|d| d.iter()
      .group_by(|&i| i)
      .into_iter()
      .any(|(_,x)| x.count() == 2)
    )
    .collect::<Vec<_>>();
  let part_one = passwords.len();
  let part_two = passwords.iter().filter(|&&b| b).count();
  (part_one,part_two)
}

fn main() {
  let now = Instant::now();
  let (part_one, part_two) = count_passwords();
  println!("Part one: {}", part_one);
  println!("Part two: {}", part_two);
  println!("Time: {}ms", now.elapsed().as_millis());
}
