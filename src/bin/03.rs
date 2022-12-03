use itertools::Itertools;

fn value(c: u8) -> usize {
  match c {
    b'a'..=b'z' => c as usize - b'a' as usize + 1,
    b'A'..=b'Z' => c as usize - b'A' as usize + 27,
    _ => unreachable!(),
  }
}

fn same_chars(a: &[u8], b: &[u8]) -> Vec<u8> {
  a.iter().copied().filter(|c| b.contains(c)).collect()
}

#[aoc::main(03)]
fn main(input: &str) -> (usize, usize) {
  let p1 = input.lines()
    .map(|l| l.as_bytes())
    .map(|l| same_chars(&l[..l.len()/2], &l[l.len()/2..]))
    .map(|c| value(c[0]))
    .sum();
  let p2 = input.lines()
    .map(|l| l.as_bytes())
    .tuples()
    .map(|(a,b,c)| same_chars(a, &same_chars(b, c)))
    .map(|c| value(c[0]))
    .sum();
  (p1, p2)
}
