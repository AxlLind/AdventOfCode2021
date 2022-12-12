use std::collections::VecDeque;
use itertools::Itertools;

fn bfs(grid: &[Vec<u8>], start: &[(usize, usize)], goal: (usize, usize)) -> Option<usize> {
  let mut visited = vec![vec![false; grid[0].len()]; grid.len()];
  let mut queue = start.iter().map(|&s| (s, 0)).collect::<VecDeque<_>>();
  while let Some(((x, y), len)) = queue.pop_front() {
    if (x, y) == goal {
      return Some(len);
    }
    for (dx, dy) in [(0,-1), (-1,0), (0,1), (1,0)] {
      let (nx, ny) = ((x as isize + dx) as usize, (y as isize + dy) as usize);
      let Some(&square) = grid.get(nx).and_then(|row| row.get(ny)) else { continue };
      if grid[x][y] + 1 >= square && !visited[nx][ny] {
        visited[nx][ny] = true;
        queue.push_back(((nx, ny), len + 1));
      }
    }
  }
  None
}

#[aoc::main(12)]
fn main(input: &str) -> (usize, usize) {
  let mut grid = input.lines().map(|l| l.as_bytes().iter().copied().collect::<Vec<_>>()).collect::<Vec<_>>();
  let (sx, sy) = (0..grid.len()).cartesian_product(0..grid[0].len()).find(|&(x,y)| grid[x][y] == b'S').unwrap();
  let (gx, gy) = (0..grid.len()).cartesian_product(0..grid[0].len()).find(|&(x,y)| grid[x][y] == b'E').unwrap();
  grid[sx][sy] = b'a';
  grid[gx][gy] = b'z';
  let positions = (0..grid.len()).cartesian_product(0..grid[0].len())
    .filter(|&(x,y)| grid[x][y] == b'a')
    .collect::<Vec<_>>();
  (bfs(&grid, &[(sx, sy)], (gx, gy)).unwrap(), bfs(&grid, &positions, (gx, gy)).unwrap())
}
