use hashbrown::HashSet;
use itertools::Itertools;

fn flash(flashed: &mut HashSet<(usize,usize)>, map: &mut [Vec<u8>], c: usize, r: usize) {
  if map[r][c] < 10 || !flashed.insert((r,c)) {
    return;
  }
  for (r,c) in [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)] {
    if r < 10 && c < 10 {
      map[r][c] += 1;
      flash(flashed, map, c, r);
    }
  }
}

fn round(map: &mut [Vec<u8>]) -> usize {
  let mut flashed = HashSet::new();
  for (r,c) in (0..10).cartesian_product(0..10) {
    map[r][c] += 1;
  }
  for (r,c) in (0..10).cartesian_product(0..10) {
    flash(&mut flashed, map, c, r);
  }
  for (r,c) in (0..10).cartesian_product(0..10) {
    if map[r][c] > 9 { map[r][c] = 0 }
  }
  flashed.len()
}

#[aoc::main(11)]
fn main(input: &str) -> (usize,usize) {
  let mut map = input.lines()
    .map(|l| l.bytes().map(|b| b - b'0').collect::<Vec<_>>())
    .collect::<Vec<_>>();
  let p1 = (0..100).map(|_| round(&mut map)).sum::<usize>();
  let p2 = (100..).find(|_| round(&mut map) == 100).unwrap();
  (p1,p2+1)
}
