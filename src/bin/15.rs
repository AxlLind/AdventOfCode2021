use itertools::Itertools;

fn part_one(beacons: &[(i64,i64,i64)]) -> i64 {
  let compressed = beacons.iter()
    .map(|&(x,y,d)| (x, d - (2000000 - y).abs()))
    .filter(|&(_,left)| left >= 0)
    .flat_map(|(x,left)| [(x - left, true), (x + left + 1, false)])
    .sorted()
    .collect::<Vec<_>>();
  let (mut ans, mut prev, mut inside) = (-1, 0, 0);
  for &(x, start) in &compressed {
    if inside > 0 { ans += x - prev }
    inside += if start {1} else {-1};
    prev = x;
  }
  ans
}

fn part_two(beacons: &[(i64,i64,i64)]) -> i64 {
  for &(x,y,d) in beacons {
    for (dir_x, dir_y) in [(-1,-1), (-1,1), (1,-1), (1,1)] {
      for dist in 0..d {
        let bx = x + dir_x * dist;
        let by = y + dir_y * (d + 1 - dist);
        if bx < 0 || by < 0 || bx > 4000000 || by > 4000000 {
          break;
        }
        if beacons.iter().all(|&(x,y,d)| (bx - x).abs() + (by - y).abs() >= d) {
          return bx * 4000000 + by;
        }
      }
    }
  }
  unreachable!()
}

#[aoc::main(15)]
fn main(input: &str) -> (i64, i64) {
  let beacons = input.lines().map(|l|
    l.split(|c: char| !c.is_digit(10) && c != '-')
      .filter_map(|w| w.parse::<i64>().ok())
      .collect_tuple()
      .map(|(x,y,dx,dy)| (x, y, (x - dx).abs() + (y - dy).abs()))
      .unwrap()
  ).collect::<Vec<_>>();
  (part_one(&beacons), part_two(&beacons))
}
