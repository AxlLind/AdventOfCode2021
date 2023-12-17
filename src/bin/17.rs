use std::collections::BinaryHeap;
use hashbrown::HashMap;

fn dijkstra(grid: &[&[u8]], minstep: isize, maxstep: isize) -> i64 {
  let mut dists = HashMap::new();
  let mut q = BinaryHeap::from_iter([(0, (0,0,(0,0)))]);
  while let Some((cost, (r, c, d))) = q.pop() {
    if (r,c) == (grid.len() - 1, grid[0].len() - 1) {
      return -cost;
    }
    if dists.get(&(r, c, d)).is_some_and(|&c| -cost > c) {
      continue;
    }
    for (dr, dc) in [(-1,0), (1,0), (0,-1), (0,1)] {
      if d == (dr, dc) || d == (-dr, -dc) {
        continue;
      }
      let mut next_cost = -cost;
      for dist in 1..=maxstep {
        let rr = (r as isize + dr * dist) as usize;
        let cc = (c as isize + dc * dist) as usize;
        if rr >= grid.len() || cc >= grid[0].len() {
          continue;
        }
        next_cost += (grid[rr][cc] - b'0') as i64;
        if dist < minstep {
          continue;
        }
        let key = (rr, cc, (dr, dc));
        if next_cost < *dists.get(&key).unwrap_or(&i64::MAX) {
          dists.insert(key, next_cost);
          q.push((-next_cost, key));
        }
      }
    }
  }
  unreachable!()
}

#[aoc::main(17)]
fn main(input: &str) -> (i64, i64) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  (dijkstra(&grid, 1, 3), dijkstra(&grid, 4, 10))
}
