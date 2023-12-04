#[aoc::main(04)]
fn main(input: &str) -> (usize, usize) {
  let cards = input.split('\n').map(|l| {
    let (_, rest) = l.split_once(": ").unwrap();
    let (wanted, got) = rest.split_once(" | ").unwrap();
    let wanted = wanted.split_whitespace()
      .map(|w| w.parse::<usize>().unwrap())
      .collect::<Vec<_>>();
    let got = got.split_whitespace()
      .map(|w| w.parse::<usize>().unwrap())
      .collect::<Vec<_>>();
    (wanted, got)
  }).collect::<Vec<_>>();

  let mut p1 = 0;
  let mut copies = vec![1; cards.len()];
  for (i, (wanted, got)) in cards.iter().enumerate() {
    let won = got.iter().filter(|c| wanted.contains(c)).count();
    p1 += if won != 0 {1 << (won - 1)} else {0};
    for j in 0..won {
      copies[i+j+1] += copies[i];
    }
  }
  (p1, copies.iter().sum())
}
