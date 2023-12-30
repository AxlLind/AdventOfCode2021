const STR_DIGITS: &[&[u8]] = &[b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine"];

fn digit_sum(w: &[u8], part_two: bool) -> usize {
  let mut digits = (0..w.len()).filter_map(|i| match w[i] {
    b'0'..=b'9' => Some((w[i] - b'0') as usize),
    _ if part_two => STR_DIGITS.iter()
      .enumerate()
      .find_map(|(di, d)| w[i..].starts_with(d).then_some(di + 1)),
    _ => None
  });
  let a = digits.next().unwrap();
  let b = digits.last().unwrap_or(a);
  a * 10 + b
}

#[aoc::main(01)]
fn main(input: &str) -> (usize, usize) {
  let (mut p1, mut p2) = (0,0);
  for l in input.split('\n') {
    p1 += digit_sum(l.as_bytes(), false);
    p2 += digit_sum(l.as_bytes(), true);
  }
  (p1, p2)
}
