use itertools::Itertools;
use hashbrown::HashMap;

fn num_overlapping(lines: impl Iterator<Item=(i32,i32,i32,i32)>) -> usize {
  let mut points = HashMap::new();
  for (x1,y1,x2,y2) in lines {
    let dx = (x2 - x1).signum();
    let dy = (y2 - y1).signum();
    let (mut x, mut y) = (x1,y1);
    while (x,y) != (x2+dx,y2+dy) {
      *points.entry((x,y)).or_insert(0) += 1;
      x += dx;
      y += dy;
    }
  }
  points.values().filter(|&&n| n > 1).count()
}

#[aoc::main("05")]
fn main(input: &str) -> (usize,usize) {
  let lines = input.lines()
    .filter_map(|l| l.split(" -> ")
      .map(|s| s.split(','))
      .flatten()
      .map(|i| i.parse().unwrap())
      .collect_tuple()
    )
    .collect::<Vec<_>>();
  let p1 = num_overlapping(lines.iter().copied().filter(|(x1,y1,x2,y2)| x1 == x2 || y1 == y2));
  let p2 = num_overlapping(lines.iter().copied());
  (p1,p2)
}
