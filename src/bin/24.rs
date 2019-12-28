use std::time::Instant;
use std::collections::{HashSet, HashMap};
use itertools::Itertools;

static INPUT: [[char;5]; 5] = [
  ['#','#','#','#','#'],
  ['.','.','.','#','#'],
  ['#','.','.','#','.'],
  ['#','.','.','.','.'],
  ['#','.','.','.','#'],
];

fn bug_at(map: &[[char;5]], (x,y): (usize,usize), (dx,dy): (i64,i64)) -> bool {
  let new_x = x as i64 + dx;
  let new_y = y as i64 + dy;
  if !(0..5).contains(&new_x) { return false; }
  if !(0..5).contains(&new_y) { return false; }
  map[new_x as usize][new_y as usize] == '#'
}

fn part_one() -> usize {
  let mut cells = INPUT;
  let neighbors = [(0,-1), (0,1), (-1,0), (1,0)];
  let mut seen = HashSet::new();
  loop {
    let mut new_cells = cells;
    for (x,y) in (0..5).cartesian_product(0..5) {
      let n = neighbors.iter()
        .filter(|&&diff| bug_at(&cells, (x,y), diff))
        .count();
      let is_bug = cells[x][y] == '#';
      if is_bug && n != 1 { new_cells[x][y] = '.'; }
      if !is_bug && (n == 1 || n == 2) { new_cells[x][y] = '#'; }
    }
    cells = new_cells;
    if seen.contains(&cells) { break; }
    seen.insert(cells);
  }
  (0..5).cartesian_product(0..5)
    .filter(|&(x,y)| cells[x][y] == '#')
    .map(|(x,y)| 1 << (x * 5 + y))
    .sum::<usize>()
}

fn count_neighbours(bugs: &HashSet<(u8,u8,i32)>) -> HashMap<(u8,u8,i32),usize> {
  let mut count = bugs.iter()
    .map(|&pos| (pos,0))
    .collect::<HashMap<_,_>>();

  macro_rules! neighbour_to {
    ($x:expr, $y:expr, $z:expr) => {
      *count.entry(($x,$y,$z)).or_insert(0) += 1;
    }
  }

  for &(x,y,z) in bugs {
    match x {
      0 => neighbour_to!(2, 1, z-1),
      4 => neighbour_to!(2, 3, z-1),
      _ => {},
    }
    match y {
      0 => neighbour_to!(1, 2, z-1),
      4 => neighbour_to!(3, 2, z-1),
      _ => {},
    }
    match (x,y) {
      (1,2) => for x in 0..5 { neighbour_to!(x, 0, z+1); }
      (3,2) => for x in 0..5 { neighbour_to!(x, 4, z+1); }
      (2,1) => for y in 0..5 { neighbour_to!(0, y, z+1); }
      (2,3) => for y in 0..5 { neighbour_to!(4, y, z+1); }
      _     => {},
    }
    if x != 0 && (x-1 != 2 || y != 2) { neighbour_to!(x-1, y, z); }
    if x != 4 && (x+1 != 2 || y != 2) { neighbour_to!(x+1, y, z); }
    if y != 0 && (x != 2 || y-1 != 2) { neighbour_to!(x, y-1, z); }
    if y != 4 && (x != 2 || y+1 != 2) { neighbour_to!(x, y+1, z); }
  }
  count
}

fn part_two() -> usize {
  let mut bugs = INPUT.iter()
    .enumerate()
    .flat_map(|(y,s)| s.iter()
      .enumerate()
      .filter(|&(_,tile)| *tile == '#')
      .map(|(x,_)| (x as u8, y as u8, 0))
      .collect_vec()
    )
    .collect::<HashSet<_>>();
  for _ in 0..200 {
    for (&pos, &n) in &count_neighbours(&bugs) {
      let is_bug = bugs.contains(&pos);
      if is_bug && n != 1 { bugs.remove(&pos); }
      if !is_bug && (n == 1 || n == 2) { bugs.insert(pos); }
    }
  }
  bugs.len()
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
