const STR_DIGITS: &[&[u8]] = &[b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine"];

fn p1_digits(l: &[u8], i: usize) -> Option<usize> {
  (b'0'..=b'9').contains(&l[i]).then_some((l[i] - b'0') as usize)
}

fn p2_digits(l: &[u8], i: usize) -> Option<usize> {
  if let Some(d) = p1_digits(l, i) {
    return Some(d);
  }
  STR_DIGITS.iter()
    .enumerate()
    .find_map(|(di, d)| l[i..].starts_with(d).then_some(di + 1))
}

fn get_digit_sum(lines: &[&[u8]], f: impl Fn(&[u8], usize) -> Option<usize>) -> usize {
  lines.iter().map(|l| {
    let mut digits = (0..l.len()).filter_map(|i| f(l, i));
    let a = digits.next().unwrap();
    let b = digits.last().unwrap_or(a);
    a * 10 + b
  }).sum()
}

#[aoc::main(01)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let p1 = get_digit_sum(&lines, p1_digits);
  let p2 = get_digit_sum(&lines, p2_digits);
  (p1, p2)
}
