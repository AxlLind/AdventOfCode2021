use itertools::Itertools;

fn part_one(beacons: &[(i64,i64,i64,i64)]) -> i64 {
  let mut ends = vec![];
  for &(x,y,dx,dy) in beacons {
    let left = (x - dx).abs() + (y - dy).abs() - (2000000 - y).abs();
    if left >= 0 {
      ends.extend_from_slice(&[(x - left, true), (x + left + 1, false)]);
    }
  }
  ends.sort();
  let (mut ans, mut prev, mut inside) = (-1, 0, 0);
  for &(x, start) in &ends {
    if inside > 0 { ans += x - prev }
    inside += if start {1} else {-1};
    prev = x;
  }
  ans
}

fn part_two(beacons: &[(i64,i64,i64,i64)]) -> i64 {
  for &(x,y,dx,dy) in beacons {
    let dist = (x - dx).abs() + (y - dy).abs();
    for (dir_x, dir_y) in [(-1,-1), (-1,1), (1,-1), (1,1)] {
      for d in 0..dist {
        let bx = x + dir_x * d;
        let by = y + dir_y * (dist + 1 - d);
        if bx < 0 || by < 0 || bx > 4000000 || by > 4000000 {
          break;
        }
        let found = beacons.iter().all(|&(x,y,dx,dy)| {
          let d1 = (x - dx).abs() + (y - dy).abs();
          let d2 = (bx - x).abs() + (by - y).abs();
          d2 >= d1
        });
        if found { return bx * 4000000 + by; }
      }
    }
  }
  unreachable!()
}

#[aoc::main(15)]
fn main(input: &str) -> (i64, i64) {
  let beacons = input.lines().map(|l|
    l.split(|c| !"-0123456789".contains(c))
      .filter(|w| !w.is_empty())
      .map(|w| w.parse::<i64>().unwrap())
      .collect_tuple()
      .unwrap()
  ).collect::<Vec<_>>();
  (part_one(&beacons), part_two(&beacons))
}
