use std::cmp::max;
use itertools::Itertools;

fn examine_tree(grid: &[Vec<u8>], r: usize, c: usize) -> (bool, usize) {
  let tree = grid[r][c];
  let (mut invisible, mut score) = (true, 1);
  for (dr, dc) in [(-1,0), (0,-1), (1,0), (0,1)] {
    let (mut r, mut c, mut i, mut visible) = (r, c, 0, true);
    while let Some(&next) = grid.get((r as isize + dr) as usize).and_then(|x| x.get((c as isize + dc) as usize)) {
      i += 1;
      if tree <= next {
        visible = false;
        break;
      }
      r = (r as isize + dr) as usize;
      c = (c as isize + dc) as usize;
    }
    if visible {
      invisible = false;
    }
    score *= i;
  }
  (!invisible, score)
}

#[aoc::main(08)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.lines()
    .map(|l| l.as_bytes().iter().map(|b| b - b'0').collect::<Vec<_>>())
    .collect::<Vec<_>>();
  (0..grid.len()).cartesian_product(0..grid[0].len()).fold((0, 0), |(p1, p2), (r, c)| {
    let (visible, score) = examine_tree(&grid, r, c);
    (p1 + visible as usize, max(p2, score))
  })
}
