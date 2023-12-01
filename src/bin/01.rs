const STR_DIGITS: &[&[u8]] = &[b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine"];

fn digit_sum(w: &[u8], part_two: bool) -> usize {
  let mut digits = (0..w.len()).filter_map(|i| {
    if (b'0'..=b'9').contains(&w[i]) {
      return Some((w[i] - b'0') as usize);
    }
    if !part_two {
      return None
    }
    STR_DIGITS.iter()
      .enumerate()
      .find_map(|(di, d)| w[i..].starts_with(d).then_some(di + 1))
  });
  let a = digits.next().unwrap();
  let b = digits.last().unwrap_or(a);
  a * 10 + b
}

#[aoc::main(01)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let p1 = lines.iter().map(|line| digit_sum(line, false)).sum();
  let p2 = lines.iter().map(|line| digit_sum(line, true)).sum();
  (p1, p2)
}
