fn calc_area(instructions: impl Iterator<Item=(u8, isize)>) -> isize {
  let (mut r, mut c, mut a) = (0,0,0);
  for (d, n) in instructions {
    let (rr, cc) = (r,c);
    match d {
      b'U' => r -= n,
      b'R' => c += n,
      b'D' => r += n,
      b'L' => c -= n,
      _ => unreachable!(),
    }
    a += (c + cc) * (r - rr) + n;
  }
  a / 2 + 1
}

#[aoc::main(18)]
fn main(input: &str) -> (isize, isize) {
  let p1 = input.split('\n').map(|l| {
    let (n, _) = l[2..].split_once(' ').unwrap();
    (l.as_bytes()[0], n.parse().unwrap())
  });
  let p2 = input.split('\n').map(|l| {
    let (_, color) = l.split_once('#').unwrap();
    let d = b"RDLU"[(color.as_bytes()[color.len()-2] - b'0') as usize];
    (d, isize::from_str_radix(&color[0..color.len()-2], 16).unwrap())
  });
  (calc_area(p1), calc_area(p2))
}
