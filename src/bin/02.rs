fn score(rounds: &[(char, char)]) -> usize {
  rounds.iter().map(|(a,b)| match (a,b) {
    ('A', 'Y') => 6 + 2,
    ('B', 'Z') => 6 + 3,
    ('C', 'X') => 6 + 1,
    ('A', 'X') => 3 + 1,
    ('B', 'Y') => 3 + 2,
    ('C', 'Z') => 3 + 3,
    ('A', 'Z') => 0 + 3,
    ('B', 'X') => 0 + 1,
    ('C', 'Y') => 0 + 2,
    _ => unreachable!(),
  }).sum()
}

#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  let r1 = input.lines()
    .map(|l| (l.as_bytes()[0] as char, l.as_bytes()[2] as char))
    .collect::<Vec<_>>();
  let r2 = r1.iter().copied().map(|(a,b)| match (a,b) {
    // lose
    ('A', 'X') => ('A', 'Z'),
    ('B', 'X') => ('B', 'X'),
    ('C', 'X') => ('C', 'Y'),
    // draw
    ('A', 'Y') => ('A', 'X'),
    ('B', 'Y') => ('B', 'Y'),
    ('C', 'Y') => ('C', 'Z'),
    // win
    ('A', 'Z') => ('A', 'Y'),
    ('B', 'Z') => ('B', 'Z'),
    ('C', 'Z') => ('C', 'X'),
    _ => unreachable!()
  }).collect::<Vec<_>>();
  (score(&r1), score(&r2))
}
