use hashbrown::HashSet;
use itertools::Itertools;

static INPUT: [[u8;10];10] = [[4,7,8,1,6,2,3,8,8,8],[1,7,8,4,1,5,6,1,1,4],[3,2,6,5,6,4,5,1,2,2],[4,3,7,1,5,5,1,4,1,4],[3,3,7,7,1,5,4,8,8,6],[7,8,8,2,3,1,4,4,5,5],[6,4,2,1,3,4,8,6,8,1],[7,1,7,5,4,2,4,2,8,7],[5,4,8,8,2,4,2,1,8,4],[2,4,4,8,5,6,8,2,6,1]];

fn flash(flashed: &mut HashSet<(usize,usize)>, map: &mut [[u8;10];10], c: usize, r: usize) {
  if map[r][c] < 10 || !flashed.insert((r,c)) {
    return;
  }
  let neighbours = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)];
  for &(r,c) in &neighbours {
    if map.get(r).and_then(|row| row.get(c)).is_none() {
      continue;
    }
    map[r][c] += 1;
    flash(flashed, map, c, r);
  }
}

fn round(map: &mut [[u8;10];10]) -> usize {
  let mut flashed = HashSet::new();
  for (r,c) in (0..10).cartesian_product(0..10) {
    map[r][c] += 1;
  }
  for (r,c) in (0..10).cartesian_product(0..10) {
    flash(&mut flashed, map, c, r);
  }
  for (r,c) in (0..10).cartesian_product(0..10) {
    if map[r][c] > 9 {
      map[r][c] = 0;
    }
  }
  flashed.len()
}

aoc2021::main! {
  let mut map = INPUT;
  let p1 = (0..100).map(|_| round(&mut map)).sum::<usize>();
  let p2 = (100..).find(|_| round(&mut map) == 100).unwrap();
  (p1,p2)
}
