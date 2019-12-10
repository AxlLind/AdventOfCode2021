use std::collections::HashSet;
use std::f64::consts::PI;
use itertools::Itertools;
use num_integer::gcd;

static INPUT: [&str; 33] = [
  "..#..###....#####....###........#",
  ".##.##...#.#.......#......##....#",
  "#..#..##.#..###...##....#......##",
  "..####...#..##...####.#.......#.#",
  "...#.#.....##...#.####.#.###.#..#",
  "#..#..##.#.#.####.#.###.#.##.....",
  "#.##...##.....##.#......#.....##.",
  ".#..##.##.#..#....#...#...#...##.",
  ".#..#.....###.#..##.###.##.......",
  ".##...#..#####.#.#......####.....",
  "..##.#.#.#.###..#...#.#..##.#....",
  ".....#....#....##.####....#......",
  ".#..##.#.........#..#......###..#",
  "#.##....#.#..#.#....#.###...#....",
  ".##...##..#.#.#...###..#.#.#..###",
  ".#..##..##...##...#.#.#...#..#.#.",
  ".#..#..##.##...###.##.#......#...",
  "...#.....###.....#....#..#....#..",
  ".#...###..#......#.##.#...#.####.",
  "....#.##...##.#...#........#.#...",
  "..#.##....#..#.......##.##.....#.",
  ".#.#....###.#.#.#.#.#............",
  "#....####.##....#..###.##.#.#..#.",
  "......##....#.#.#...#...#..#.....",
  "...#.#..####.##.#.........###..##",
  ".......#....#.##.......#.#.###...",
  "...#..#.#.........#...###......#.",
  ".#.##.#.#.#.#........#.#.##..#...",
  ".......#.##.#...........#..#.#...",
  ".####....##..#..##.#.##.##..##...",
  ".#.#..###.#..#...#....#.###.#..#.",
  "............#...#...#.......#.#..",
  ".........###.#.....#..##..#.##...",
];
static H: i64 = INPUT.len() as i64;
static W: i64 = INPUT[0].len() as i64;

fn map_to_astroid_coords() -> HashSet<(i64,i64)> {
  INPUT.iter()
    .enumerate()
    .flat_map(|(j,s)| s.chars()
      .enumerate()
      .filter(|&(_,c)| c != '.')
      .map(|(i,_)| (i as i64, j as i64))
      .collect_vec()
    )
    .collect()
}

fn to_angle((x,y): (i64,i64)) -> f64 {
  let d = (y as f64).atan2(x as f64) + PI / 2.0;
  if d < 0.0 { 2.0 * PI + d } else { d }
}

fn unique_lines_sorted_by_angle() -> Vec<(i64,i64)> {
  let (x_max, y_max) = (W-1,H-1);
  (-x_max..x_max)
    .cartesian_product(-y_max..y_max)
    .filter(|&(x,y)| gcd(x,y) == 1)
    .sorted_by(|&p1, &p2| {
      let v1 = to_angle(p1);
      let v2 = to_angle(p2);
      v1.partial_cmp(&v2).unwrap()
    })
    .collect()
}

fn until_hit(
  asteroids: &HashSet<(i64,i64)>,
  (x,y): (i64,i64),
  (dx,dy): (i64,i64),
) -> Option<(i64,i64)> {
  let (mut new_x, mut new_y) = (x,y);
  while (0..H).contains(&new_x) && (0..W).contains(&new_y) {
    new_x += dx;
    new_y += dy;
    if asteroids.contains(&(new_x, new_y)) {
      return Some((new_x, new_y));
    }
  }
  None
}

fn main() {
  let mut asteroids = map_to_astroid_coords();
  let lines = unique_lines_sorted_by_angle();

  let station = (27,19);
  let mut num_hit = 0;
  for &slope in lines.iter().cycle() {
    if let Some(hit) = until_hit(&asteroids, station, slope) {
      asteroids.remove(&hit);
      num_hit += 1;
      if num_hit == 200 {
        println!("{:?}", hit);
        break;
      }
    }
  }
}
