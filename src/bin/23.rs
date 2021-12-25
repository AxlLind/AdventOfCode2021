use hashbrown::HashMap;
use std::collections::BinaryHeap;
use itertools::Itertools;

static INPUT: &str = "#############\n#...........#\n###C#A#B#D###\n  #D#C#A#B#  \n  #########  ";

fn right_configuration(maze: &Vec<Vec<u8>>) -> bool {
  maze.iter().skip(1).all(|l| itertools::equal(l[3..10].iter().copied(), "A#B#C#D".bytes()))
}

fn make_move(maze: &Vec<Vec<u8>>, x0: usize, y0: usize, x1: usize, y1: usize) -> Vec<Vec<u8>> {
  let mut m = maze.clone();
  m[x1][y1] = m[x0][y0];
  m[x0][y0] = b'.';
  m
}

fn moves(maze: &Vec<Vec<u8>>) -> Vec<(usize,Vec<Vec<u8>>)> {
  let room_len = maze.len() - 1;
  let mut moves = Vec::new();
  for y in 0..maze[0].len() {
    // check moving into a room
    let (room,exp) = match maze[0][y] {
      b'A' => (3,1),
      b'B' => (5,10),
      b'C' => (7,100),
      b'D' => (9,1000),
      _ => continue,
    };
    let (r0,r1) = if y > room {(room,y)} else {(y+1,room+1)};
    if (r0..r1).any(|i| maze[0][i] != b'.') {
      continue;
    }
    let i = match (1..=room_len).take_while(|&i| maze[i][room] == b'.').last() {
      Some(i) => i,
      _ => continue
    };
    if i != room_len && maze[i+1][room] != maze[0][y] { continue; }
    moves.push(((r1-r0 + i) * exp, make_move(maze, 0, y, i, room)));
  }
  for (x,y) in (1..=room_len).cartesian_product([3,5,7,9]) {
    // check moving out of a room
    let exp = match maze[x][y] {
      b'A' => 1,
      b'B' => 10,
      b'C' => 100,
      b'D' => 1000,
      _ => continue,
    };
    if (1..x).any(|i| maze[i][y] != b'.') || (x+1..=room_len).any(|i| maze[i][y] == b'.') {
      continue;
    }
    for i in y..maze[0].len() { // move left
      if maze[0][i] != b'.' { break; }
      if ![1,2,4,6,8,10,11].contains(&i) { continue; }
      moves.push(((x + i-y) * exp, make_move(maze, x, y, 0, i)));
    }
    for i in (1..=y).rev() { // move right
      if maze[0][i] != b'.' { break; }
      if ![1,2,4,6,8,10,11].contains(&i) { continue; }
      moves.push(((x + y-i) * exp,make_move(maze, x, y, 0, i)));
    }
  }
  moves
}

fn shortest_path(maze: &Vec<Vec<u8>>) -> i64 {
  let mut dist = HashMap::new();
  let mut q = BinaryHeap::new();
  q.push((0,maze.clone()));
  while let Some((cost,m)) = q.pop() {
    if right_configuration(&m) { return -cost; }
    if let Some(&c) = dist.get(&m) {
      if -cost > c { continue; }
    }
    for (nmoves, m) in moves(&m) {
      let next_cost = -cost + nmoves as i64;
      let &c = dist.get(&m).unwrap_or(&1000000);
      if c > next_cost {
        dist.insert(m.clone(), next_cost);
        q.push((-next_cost,m));
      }
    }
  }
  unreachable!()
}

aoc2021::main! {
  let mut map = INPUT.lines().skip(1).take(3).map(|l| l.bytes().collect()).collect();
  let p1 = shortest_path(&map);
  map.push("  #D#C#B#A#  ".bytes().collect());
  map.push("  #D#B#A#C#  ".bytes().collect());
  let p2 = shortest_path(&map);
  (p1,p2)
}
