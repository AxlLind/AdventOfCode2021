use itertools::Itertools;

fn solve(universe: &Vec<Vec<u8>>, mut galaxies: Vec<(usize, usize)>, size: usize) -> usize {
  let empty_rows = (0..universe.len()).rev()
    .filter(|&r| universe[r].iter().all(|&c| c == b'.'));
  let empty_cols = (0..universe[0].len()).rev()
    .filter(|&c| (0..universe.len()).all(|r| universe[r][c] == b'.'));
  for r in empty_rows {
    for g in &mut galaxies {
      if g.0 > r { g.0 += size - 1 }
    }
  }
  for c in empty_cols {
    for g in &mut galaxies {
      if g.1 > c { g.1 += size - 1 }
    }
  }
  galaxies.iter()
    .tuple_combinations()
    .map(|(&(r1,c1), &(r2,c2))| r1.abs_diff(r2) + c1.abs_diff(c2))
    .sum()
}

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
  let universe = input.split('\n')
    .map(|l| l.as_bytes().to_vec())
    .collect::<Vec<_>>();
  let galaxies = (0..universe.len())
    .cartesian_product(0..universe[0].len())
    .filter(|&(r,c)| universe[r][c] == b'#')
    .collect::<Vec<_>>();
  let p1 = solve(&universe, galaxies.clone(), 2);
  let p2 = solve(&universe, galaxies, 1000000);
  (p1, p2)
}
