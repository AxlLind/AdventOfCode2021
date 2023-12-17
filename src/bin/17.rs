use std::collections::BinaryHeap;
use hashbrown::HashMap;

fn shortest_path(grid: &[&[u8]], minstep: isize, maxstep: isize) -> i64 {
  let mut dists = HashMap::new();
  let mut q = BinaryHeap::from_iter([(0, (0,0,(0,0)))]);
  while let Some((cost, (r, c, d))) = q.pop() {
    if (r,c) == (grid.len() - 1, grid[0].len() - 1) {
      return -cost;
    }
    if let Some(&c) = dists.get(&(r, c, d)) {
      if -cost > c { continue }
    }
    for (dr, dc) in [(-1,0), (1,0), (0,-1), (0,1)] {
      if (dr, dc) == d || (-dr, -dc) == d {
        continue;
      }
      let mut extra_cost = 0;
      for dist in 1..=maxstep {
        let rr = (r as isize + dr * dist) as usize;
        let cc = (c as isize + dc * dist) as usize;
        if rr >= grid.len() || cc >= grid[0].len() {
          continue;
        }
        extra_cost += (grid[rr][cc] - b'0') as i64;
        if dist < minstep {
          continue;
        }
        let next_cost = -cost + extra_cost;
        let key = (rr, cc, (dr, dc));
        if next_cost < *dists.get(&key).unwrap_or(&10000000) {
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
  let xs = input.split('\n').map(|l| {
    l.as_bytes()
  }).collect::<Vec<_>>();
  (shortest_path(&xs, 1, 3), shortest_path(&xs, 4, 10))
}
