fn step(r: usize, c: usize, d: usize) -> (usize, usize, usize) {
  let (dr, dc) = [(-1,0),(0,1),(1,0),(0,-1)][d];
  ((r as isize + dr) as _, (c as isize + dc) as _, d)
}

fn energized_tiles(grid: &[&[u8]], start: (usize,usize,usize)) -> usize {
  let mut seen = vec![vec![[false; 4]; grid[0].len()]; grid.len()];
  let (mut beams, mut new_beams) = (vec![start], vec![]);
  while !beams.is_empty() {
    new_beams.clear();
    for &(r,c,d) in &beams {
      if r >= grid.len() || c >= grid[0].len() || seen[r][c][d] {
        continue;
      }
      seen[r][c][d] = true;
      match (grid[r][c], d) {
        (b'|', 1|3) => new_beams.extend([step(r,c,0), step(r,c,2)]),
        (b'-', 0|2) => new_beams.extend([step(r,c,1), step(r,c,3)]),
        (b'/',   _) => new_beams.push(step(r,c,[1,0,3,2][d])),
        (b'\\',  _) => new_beams.push(step(r,c,[3,2,1,0][d])),
        _           => new_beams.push(step(r,c,d)),
      }
    }
    (beams, new_beams) = (new_beams, beams);
  }
  seen.iter().flatten().filter(|x| x.iter().any(|&b| b)).count()
}

#[aoc::main(16)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let p2 = (0..grid.len()).flat_map(|r| [(r,0,1), (r,grid[0].len()-1,3)])
    .chain((0..grid[0].len()).flat_map(|c| [(0,c,2), (grid.len()-1,c,0)]))
    .map(|start| energized_tiles(&grid, start))
    .max()
    .unwrap();
  (energized_tiles(&grid, (0,0,1)), p2)
}
