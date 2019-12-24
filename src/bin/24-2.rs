use std::time::Instant;
use std::collections::{HashSet, HashMap};
use itertools::Itertools;

static INPUT: [&str; 5] = [
  "#####",
  "...##",
  "#..#.",
  "#....",
  "#...#",
];

fn count_neighbours(bugs: &HashSet<(u8,u8,i32)>) -> HashMap<(u8,u8,i32),usize> {
  let mut count = bugs.iter()
    .map(|&pos| (pos,0))
    .collect::<HashMap<_,_>>();

  macro_rules! add_us {
    ($x:expr, $y:expr, $z:expr) => {
      *count.entry(($x,$y,$z)).or_insert(0) += 1;
    }
  }

  for &(x,y,z) in bugs {
    match x {
      0 => add_us!(2, 1, z-1),
      4 => add_us!(2, 3, z-1),
      _ => {},
    }
    match y {
      0 => add_us!(1, 2, z-1),
      4 => add_us!(3, 2, z-1),
      _ => {},
    }
    match (x,y) {
      (1,2) => for x in 0..5 { add_us!(x, 0, z+1); }
      (3,2) => for x in 0..5 { add_us!(x, 4, z+1); }
      (2,1) => for y in 0..5 { add_us!(0, y, z+1); }
      (2,3) => for y in 0..5 { add_us!(4, y, z+1); }
      _     => {},
    }
    if x != 0 && (x-1 != 2 || y != 2) { add_us!(x-1, y, z); }
    if x != 4 && (x+1 != 2 || y != 2) { add_us!(x+1, y, z); }
    if y != 0 && (x != 2 || y-1 != 2) { add_us!(x, y-1, z); }
    if y != 4 && (x != 2 || y+1 != 2) { add_us!(x, y+1, z); }
  }
  count
}

fn main() {
  let now = Instant::now();

  let mut bugs = INPUT.iter()
    .enumerate()
    .flat_map(|(y,s)| s.chars()
      .enumerate()
      .filter(|&(_,tile)| tile == '#')
      .map(|(x,_)| (x as u8, y as u8, 0))
      .collect_vec()
    )
    .collect::<HashSet<_>>();

  for _ in 0..200 {
    let mut new_bugs = bugs.clone();
    for (&pos, &n) in &count_neighbours(&bugs) {
      let is_bug = bugs.contains(&pos);
      if is_bug && n != 1 { new_bugs.remove(&pos); }
      if !is_bug && (n == 1 || n == 2) { new_bugs.insert(pos); }
    }
    bugs = new_bugs;
  }
  println!("Answer: {}", bugs.len());
  println!("Time: {}ms", now.elapsed().as_millis());
}
