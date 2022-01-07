use hashbrown::HashSet;
use itertools::Itertools;

fn part1(grid: &[Vec<u8>]) -> usize {
  let mut ans = 0;
  for (x,y) in (0..grid[0].len()).cartesian_product(0..grid.len()) {
    let is_low = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)].iter()
      .filter_map(|&(x,y)| grid.get(y as usize).and_then(|line| line.get(x as usize)))
      .all(|&i| i > grid[y][x]);
    if is_low { ans += grid[y][x] as usize + 1; }
  }
  ans
}

fn remove_component((x,y): (usize,usize), coords: &mut HashSet<(usize,usize)>) -> usize {
  if !coords.remove(&(x,y)) {
    return 0;
  }
  1 + [(x-1,y),(x+1,y),(x,y-1),(x,y+1)].iter()
    .map(|&neighbour| remove_component(neighbour, coords))
    .sum::<usize>()
}

fn part2(grid: &[Vec<u8>]) -> usize {
  let mut points = (0..grid[0].len()).cartesian_product(0..grid.len())
    .filter(|&(x,y)| grid[y][x] != 9)
    .collect::<HashSet<_>>();
  let mut cs = vec![];
  while let Some(&p) = points.iter().next() {
    cs.push(remove_component(p, &mut points));
  }
  cs.iter().sorted().rev().take(3).product()
}

#[aoc::main("09")]
fn main(input: &str) -> (usize,usize) {
  let grid = input.lines()
    .map(|l| l.bytes().map(|c| c - b'0').collect())
    .collect::<Vec<_>>();
  (part1(&grid), part2(&grid))
}
