use hashbrown::HashSet;

#[aoc::main(24)]
fn main(input: &str) -> (usize, usize) {
  let map = input.lines().map(|l| l.as_bytes().to_vec()).collect::<Vec<_>>();
  let (rows, cols) = (map.len(), map[0].len());
  let mut blizzards = map.iter()
    .enumerate()
    .flat_map(|(x, row)| row.iter()
      .enumerate()
      .filter_map(move |(y, b)| b">v<^".contains(b).then(|| (x,y,*b)))
    )
    .collect::<Vec<_>>();
  let mut positions = HashSet::new();
  positions.insert((0,1));
  let (mut stage, mut p1, mut p2) = (0,0,0);
  for len in 1.. {
    for b in &mut blizzards {
      match b.2 {
        b'>' => b.1 = if b.1 == cols-2 {1} else {b.1+1},
        b'<' => b.1 = if b.1 == 1 {cols-2} else {b.1-1},
        b'v' => b.0 = if b.0 == rows-2 {1} else {b.0+1},
        b'^' => b.0 = if b.0 == 1 {rows-2} else {b.0-1},
        _ => unreachable!(),
      }
    }
    let mut next_positions = HashSet::with_capacity(positions.len());
    let bs = blizzards.iter().map(|&(x,y,_)| (x,y)).collect::<HashSet<_>>();
    for &(x,y) in &positions {
      for (dx,dy) in [(1i32,0i32),(0,1),(0,0),(-1,0),(0,-1)] {
        if (x == 0 && dx == -1) || (x == rows -1 && dx == 1 ) {
          continue;
        }
        let (x,y) = (x + dx as usize, y + dy as usize);
        if (x == 0 && y != 1) || (x == rows-1 && y != cols-2) || y == 0 || y == cols-1 || bs.contains(&(x,y)) {
          continue;
        }
        next_positions.insert((x,y));
      }
    }
    positions = next_positions;
    match stage {
      0 => if positions.contains(&(rows-1, cols-2)) {
        p1 = len;
        positions.clear();
        positions.insert((rows-1, cols-2));
        stage += 1;
      }
      1 => if positions.contains(&(0,1)) {
        positions.clear();
        positions.insert((0,1));
        stage += 1;
      }
      2 => if positions.contains(&(rows-1, cols-2)) {
        p2 = len;
        break;
      }
      _ => unreachable!()
    }
  }
  (p1,p2)
}
