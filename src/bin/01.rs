const STR_DIGITS: &[&[u8]] = &[b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine"];

fn get_digit_sum(lines: &[&[u8]], part_two: bool) -> usize {
  lines.iter().map(|l| {
    let mut digits = (0..l.len()).filter_map(|i| {
      if (b'0'..=b'9').contains(&l[i]) {
        return Some((l[i] - b'0') as usize);
      }
      if !part_two {
        return None
      }
      STR_DIGITS.iter()
        .enumerate()
        .find_map(|(di, d)| l[i..].starts_with(d).then_some(di + 1))
    });
    let a = digits.next().unwrap();
    let b = digits.last().unwrap_or(a);
    a * 10 + b
  }).sum()
}

#[aoc::main(01)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let p1 = get_digit_sum(&lines, false);
  let p2 = get_digit_sum(&lines, true);
  (p1, p2)
}
