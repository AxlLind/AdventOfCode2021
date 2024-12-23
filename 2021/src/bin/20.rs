use itertools::Itertools;

fn updated_tile(m: &[u8], grid: &[Vec<u8>], r: usize, c: usize, val: u8) -> u8 {
  let ns = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)];
  let i = ns.iter().fold(0, |n, &(r,c)| {
    let x = *grid.get(r).and_then(|row| row.get(c)).unwrap_or(&val) as usize;
    n << 1 | x
  });
  (m[i] == b'#') as u8
}

fn enhance(m: &[u8], grid: &[Vec<u8>], val: u8) -> Vec<Vec<u8>> {
  let mut ans = vec![vec![0;grid[0].len()+2];grid.len()+2];
  for (r,c) in (0..ans.len()).cartesian_product(0..ans[0].len()) {
    ans[r][c] = updated_tile(m, grid, r-1, c-1, val);
  }
  ans
}

#[aoc::main(20)]
fn main(input: &str) -> (usize,usize) {
  let (m, rest) = input.split_once("\n\n").unwrap();
  let mut grid = rest.lines()
    .map(|l| l.chars().map(|c| (c == '#') as u8).collect())
    .collect::<Vec<_>>();
  for i in 0..2  { grid = enhance(m.as_bytes(), &grid, i & 1) }
  let p1 = grid.iter().flatten().filter(|&&b| b == 1).count();
  for i in 2..50 { grid = enhance(m.as_bytes(), &grid, i & 1) }
  let p2 = grid.iter().flatten().filter(|&&b| b == 1).count();
  (p1, p2)
}
