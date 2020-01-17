use std::time::Instant;
use std::collections::HashSet;
use std::f64::consts::PI;
use itertools::Itertools;
use num_integer::gcd;

const INPUT: [&str; 33] = [
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
const H: i64 = INPUT.len() as i64;
const W: i64 = INPUT[0].len() as i64;

type Pos = (i64,i64);

fn map_to_astroid_coords() -> HashSet<Pos> {
  INPUT.iter()
    .enumerate()
    .flat_map(|(j,s)| s.chars()
      .enumerate()
      .filter(|&(_,c)| c != '.')
      .map(move |(i,_)| (i as i64, j as i64))
    )
    .collect()
}

fn to_angle((x,y): Pos) -> f64 {
  let d = (y as f64).atan2(x as f64) + PI / 2.0;
  if d < 0.0 { 2.0 * PI + d } else { d }
}

fn lines_sorted_by_angle() -> Vec<Pos> {
  let (x_max, y_max) = (W-1,H-1);
  (-x_max..x_max)
    .cartesian_product(-y_max..y_max)
    .filter(|&(x,y)| gcd(x,y) == 1)
    .sorted_by(|&p1, &p2| {
      let a1 = to_angle(p1);
      let a2 = to_angle(p2);
      a1.partial_cmp(&a2).unwrap()
    })
    .collect()
}

fn until_hit(asteroids: &HashSet<Pos>, (mut x, mut y): Pos, (dx,dy): Pos) -> Option<Pos> {
  while (0..H).contains(&x) && (0..W).contains(&y) {
    x += dx;
    y += dy;
    if asteroids.contains(&(x,y)) {
      return Some((x,y));
    }
  }
  None
}

fn part_one(asteroids: &HashSet<Pos>, lines: &[Pos]) -> (Pos, usize) {
  asteroids.iter()
    .map(|&a| {
      let num_hit = lines.iter()
        .filter_map(|&slope| until_hit(&asteroids, a, slope))
        .count();
      (a, num_hit)
    })
    .max_by_key(|&(_,x)| x)
    .unwrap()
}

fn part_two(asteroids: &mut HashSet<Pos>, lines: &[Pos], station: Pos) -> i64 {
  let mut num_hit = 0;
  for &slope in lines.iter().cycle() {
    if let Some((i,j)) = until_hit(&asteroids, station, slope) {
      asteroids.remove(&(i,j));
      num_hit += 1;
      if num_hit == 200 {
        return i * 100 + j;
      }
    }
  }
  unreachable!();
}

fn main() {
  let now = Instant::now();
  let mut asteroids = map_to_astroid_coords();
  let lines = lines_sorted_by_angle();
  let (station, part_one) = part_one(&asteroids, &lines);
  let part_two = part_two(&mut asteroids, &lines, station);
  println!("Part one: {}", part_one);
  println!("Part two: {}", part_two);
  println!("Time: {}ms", now.elapsed().as_millis());
}
