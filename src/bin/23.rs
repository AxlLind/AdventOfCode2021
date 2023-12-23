fn dfs(grid: &[&[u8]], seen: &mut Vec<Vec<bool>>, (r,c): (usize, usize), dist: usize, max_dist: &mut usize, part2: bool) {
  if r == grid.len() - 1 {
    *max_dist = (*max_dist).max(dist);
  }

  let neighbours = match grid[r][c] {
    _ if part2 => [(-1,0),(1,0),(0,-1),(0,1)].as_slice(),
    b'.' => [(-1,0),(1,0),(0,-1),(0,1)].as_slice(),
    b'^' => [(-1,0)].as_slice(),
    b'>' => [(0,1)].as_slice(),
    b'v' => [(1,0)].as_slice(),
    b'<' => [(0,-1)].as_slice(),
    _ => unreachable!(),
  };
  for &(dr, dc) in neighbours {
    let rr = (r as isize + dr) as usize;
    let cc = (c as isize + dc) as usize;
    let Some(&tile) = grid.get(rr).and_then(|row| row.get(cc)) else {continue};
    if tile == b'#' || seen[rr][cc] {
      continue;
    }
    seen[rr][cc] = true;
    dfs(grid, seen, (rr, cc), dist + 1, max_dist, part2);
    seen[rr][cc] = false;
  }
}

#[aoc::main(23)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let mut seen = vec![vec![false; grid[0].len()]; grid.len()];
  let (mut p1, mut p2) = (0,0);
  dfs(&grid, &mut seen, (0,1), 0, &mut p1, false);
  dfs(&grid, &mut seen, (0,1), 0, &mut p2, true);
  (p1, p2)
}
