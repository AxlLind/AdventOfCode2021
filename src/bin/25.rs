use itertools::Itertools;

fn step(map: &mut Vec<Vec<u8>>, dr: usize, dc: usize) -> bool {
  let (rows,cols) = (map.len(), map[0].len());
  let mut map2 = vec![vec![b'.'; cols]; rows];
  let mut moved = false;
  for (r,c) in (0..rows).cartesian_product(0..cols) {
    match map[r][c] {
      b'>' if map[r][(c+dc) % cols] == b'.' => {
        map2[r][(c+dc) % cols] = b'>';
        moved = true;
      }
      b'v' if map[(r+dr) % rows][c] == b'.' => {
        map2[(r+dr) % rows][c] = b'v';
        moved = true;
      }
      b'>' => map2[r][c] = b'>',
      b'v' => map2[r][c] = b'v',
      _ => {}
    }
  }
  *map = map2;
  moved
}

#[aoc::main(25)]
fn main(input: &str) -> (usize,char) {
  let mut map = input.lines().map(|l| l.bytes().collect()).collect();
  let mut round = 0;
  loop {
    let move1 = step(&mut map, 0, 1);
    let move2 = step(&mut map, 1, 0);
    round += 1;
    if !move1 && !move2 { break }
  }
  (round,'🎄')
}
