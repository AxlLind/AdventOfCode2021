use hashbrown::HashSet;

fn find_loop(graph: &[Vec<[bool; 4]>], start: (usize, usize)) -> Option<HashSet<(usize, usize)>> {
  let (mut r, mut c) = start;
  let mut d = graph[r][c].iter().position(|&d| d).unwrap();
  let mut seen = HashSet::new();
  loop {
    if !seen.insert((r,c)) {
      break None;
    }
    let (dr, dc, came_from) = match d {
      0 => (-1, 0, 2),
      1 => ( 0, 1, 3),
      2 => ( 1, 0, 0),
      3 => ( 0,-1, 1),
      _ => unreachable!(),
    };
    r = (r as isize + dr) as usize;
    c = (c as isize + dc) as usize;
    if !graph[r][c][came_from] {
      break None;
    }
    if (r,c) == start {
      break Some(seen);
    }
    d = (0..4).find(|&i| i != came_from && graph[r][c][i]).unwrap();
  }
}

fn connections(tile: u8) -> [bool; 4] {
  match tile {
    //      [   up, right,  down,  left]
    b'|' => [ true, false,  true, false],
    b'-' => [false,  true, false,  true],
    b'L' => [ true,  true, false, false],
    b'J' => [ true, false, false,  true],
    b'7' => [false, false,  true,  true],
    b'F' => [false,  true,  true, false],
    _ => [false, false, false, false]
  }
}

#[aoc::main(10)]
fn main(input: &str) -> (usize, usize) {
  let mut start = (0,0);
  let mut graph = input.split('\n').enumerate().map(|(r, line)|
    line.bytes().enumerate().map(|(c, tile)| {
      if tile == b'S' {
        start = (r,c);
      }
      connections(tile)
    }).collect::<Vec<_>>()
  ).collect::<Vec<_>>();
  let pipe_loop = "J|-L7F".bytes().find_map(|start_tile| {
    graph[start.0][start.1] = connections(start_tile);
    find_loop(&graph, start)
  }).unwrap();
  let mut p2 = 0;
  for r in 0..graph.len() {
    let mut inside = false;
    for c in 0..graph[0].len() {
      if !pipe_loop.contains(&(r,c)) {
        p2 += inside as usize;
        continue;
      }
      if graph[r][c][0] {
        inside = !inside;
      }
    }
  }
  (pipe_loop.len() / 2, p2)
}
