use itertools::Itertools;

fn calc_area(instructions: impl Iterator<Item=(u8, isize)>) -> isize {
  let mut points = Vec::new();
  points.push((0,0));
  let (mut p, mut r, mut c) = (0, 0,0);
  for (d, n) in instructions {
    let (dr, dc) = match d {
      b'U' => (-1, 0),
      b'R' => ( 0, 1),
      b'D' => ( 1, 0),
      b'L' => ( 0,-1),
      _ => unreachable!()
    };
    r = r + dr * n;
    c = c + dc * n;
    points.push((r,c));
    p += n;
  }
  let a = points.iter()
    .tuple_windows()
    .map(|((r1,c1), (r2,c2))| (c2 + c1) * (r2 - r1))
    .sum::<isize>();
  (p + a) / 2 + 1
}

#[aoc::main(18)]
fn main(input: &str) -> (isize, isize) {
  let p1 = input.split('\n').map(|l| {
    let (n, _) = l[2..].split_once(' ').unwrap();
    (l.as_bytes()[0], n.parse::<isize>().unwrap())
  });
  let p2 = input.split('\n').map(|l| {
    let (_, color) = l.split_once(" (#").unwrap();
    let d = match color.as_bytes()[color.len()-2] {
      b'0' => b'R',
      b'1' => b'D',
      b'2' => b'L',
      b'3' => b'U',
      _ => unreachable!(),
    };
    (d, isize::from_str_radix(&color[0..color.len()-2], 16).unwrap())
  });
  (calc_area(p1), calc_area(p2))
}
