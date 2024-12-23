use itertools::Itertools;

fn part1((_,b): &(Vec<&[u8]>, Vec<&[u8]>)) -> usize {
  b.iter().filter(|x| [2,3,4,7].contains(&x.len())).count()
}

fn display_digit(perm: &[u8], s: &[u8]) -> Option<usize> {
  let mut panels = 0;
  for c in s.iter().map(|c| perm[(c - b'a') as usize]) {
    panels |= 1 << (c - b'a');
  }
  match panels {
    0b1111101 => Some(0),
    0b1100000 => Some(1),
    0b1011011 => Some(2),
    0b1111010 => Some(3),
    0b1100110 => Some(4),
    0b0111110 => Some(5),
    0b0111111 => Some(6),
    0b1101000 => Some(7),
    0b1111111 => Some(8),
    0b1111110 => Some(9),
    _ => None
  }
}

fn try_permutation(perm: &[u8], (a,b): &(Vec<&[u8]>, Vec<&[u8]>)) -> Option<usize> {
  let invalid = a.iter()
    .map(|s| display_digit(perm, s))
    .any(|o| o.is_none());
  if invalid { return None; }
  let ans = b.iter()
    .rev()
    .enumerate()
    .map(|(i,s)| display_digit(perm, s).unwrap() * 10usize.pow(i as u32))
    .sum();
  Some(ans)
}

fn part2(display: &(Vec<&[u8]>, Vec<&[u8]>)) -> usize {
  "abcdefg".bytes()
    .permutations(7)
    .find_map(|perm| try_permutation(&perm, display))
    .unwrap()
}

#[aoc::main(08)]
fn main(input: &str) -> (usize,usize) {
  let displays = input.lines()
    .map(|l| {
      let (a,b) = l.split_once(" | ").unwrap();
      let x = a.split_whitespace().map(str::as_bytes).collect();
      let y = b.split_whitespace().map(str::as_bytes).collect();
      (x,y)
    })
    .collect::<Vec<_>>();
  let p1 = displays.iter().map(part1).sum();
  let p2 = displays.iter().map(part2).sum();
  (p1,p2)
}
