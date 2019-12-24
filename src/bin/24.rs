use std::time::Instant;
use std::collections::HashSet;
use itertools::Itertools;

fn bug_at(
  map: &[[char; 5]],
  (x,y): (usize,usize),
  (dx,dy): (i64,i64)
) -> bool {
  let new_x = x as i64 + dx;
  let new_y = y as i64 + dy;
  if !(0..5).contains(&new_x) { return false; }
  if !(0..5).contains(&new_y) { return false; }
  map[new_x as usize][new_y as usize] == '#'
}

fn main() {
  let now = Instant::now();
  let mut cells = [
    ['#','#','#','#','#'],
    ['.','.','.','#','#'],
    ['#','.','.','#','.'],
    ['#','.','.','.','.'],
    ['#','.','.','.','#'],
  ];
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

  let answer: usize = (0..5).cartesian_product(0..5)
    .filter(|&(x,y)| cells[x][y] == '#')
    .map(|(x,y)| 1 << (x * 5 + y))
    .sum();

  println!("Answer {}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
