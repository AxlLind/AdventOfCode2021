use hashbrown::HashSet;
use itertools::Itertools;

const DW: [(isize, isize); 4] = [
  (-1, 0),
  ( 0, 1),
  ( 1, 0),
  ( 0,-1),
];

fn step(r: usize, c: usize, d: usize) -> (usize, usize, usize) {
  let (dr, dc) = DW[d];
  ((r as isize + dr) as usize, (c as isize + dc) as usize, d)
}

fn energized_tiles(grid: &[&[u8]], start: (usize,usize,usize)) -> usize {
  let mut seen = HashSet::new();
  let mut beams = vec![start];
  while !beams.is_empty() {
    let mut new_beams = Vec::new();
    for (r,c,d) in beams {
      if r >= grid.len() || c >= grid[0].len() {
        continue;
      }
      if !seen.insert((r,c,d)) {
        continue;
      }
      match (grid[r][c], d) {
        (b'.',   _) => new_beams.push(step(r,c,d)),
        (b'/',   _) => new_beams.push(step(r,c,[1,0,3,2][d])),
        (b'\\',  _) => new_beams.push(step(r,c,[3,2,1,0][d])),
        (b'|', 0|2) => new_beams.push(step(r,c,d)),
        (b'|',   _) => new_beams.extend([step(r,c,0), step(r,c,2)]),
        (b'-', 1|3) => new_beams.push(step(r,c,d)),
        (b'-',   _) => new_beams.extend([step(r,c,1), step(r,c,3)]),
        _ => unreachable!(),
      }
    }
    beams = new_beams;
  }
  seen.iter().map(|&(r,c,_)| (r,c)).unique().count()
}

#[aoc::main(16)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(|l| {
    l.as_bytes()
  }).collect::<Vec<_>>();
  let p2 = (0..grid.len()).flat_map(|r| [(r,0,1), (r,grid[0].len()-1,3)])
    .chain((0..grid[0].len()).flat_map(|c| [(0,c,2), (grid.len()-1,c,0)]))
    .map(|start| energized_tiles(&grid, start))
    .max()
    .unwrap();
  (energized_tiles(&grid, (0,0,1)), p2)
}
