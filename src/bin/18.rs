use itertools::Itertools;

fn calc_area(instructions: impl Iterator<Item=(char,isize)>) -> isize {
  let mut points = Vec::new();
  points.push((0,0));
  let (mut p, mut r, mut c) = (0, 0,0);
  for (d, n) in instructions {
    let (dr, dc) = match d {
      'U' => (-1, 0),
      'R' => ( 0, 1),
      'D' => ( 1, 0),
      'L' => ( 0,-1),
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
  p / 2 + a / 2 + 1
}

#[aoc::main(18)]
fn main(input: &str) -> (isize, isize) {
  let instructions = input.split('\n').map(|l| {
    let (n, color) = l[2..].split_once(" (#").unwrap();
    let color = &color[0..color.len()-1];
    let d = match color.as_bytes()[color.len()-1] {
      b'0' => 'R',
      b'1' => 'D',
      b'2' => 'L',
      b'3' => 'U',
      _ => unreachable!(),
    };
    let p1 = (l.as_bytes()[0] as char, n.parse::<isize>().unwrap());
    let p2 = (d, isize::from_str_radix(&color[0..color.len()-1], 16).unwrap());
    (p1, p2)
  }).collect::<Vec<_>>();
  (calc_area(instructions.iter().map(|&(p1,_)| p1)), calc_area(instructions.iter().map(|&(_,p2)| p2)))
}
