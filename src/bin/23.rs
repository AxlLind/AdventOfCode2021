use hashbrown::HashMap;
use std::collections::BinaryHeap;
use itertools::Itertools;

static INPUT: &str = "#############\n#...........#\n###C#A#B#D###\n  #D#C#A#B#  \n  #########  ";

fn right_configuration(maze: &Vec<Vec<char>>) -> bool {
  maze[2..(maze.len()-1)].iter().all(|l| itertools::equal(l[3..10].iter().copied(), "A#B#C#D".chars()))
}

fn moves(maze: &Vec<Vec<char>>) -> Vec<(i64,Vec<Vec<char>>)> {
  let room_len = maze.len() - 2;
  let mut moves = Vec::new();
  for y in 0..maze[1].len() {
    // check moving into a room
    let (room,exp) = match maze[1][y] {
      'A' => (3,1),
      'B' => (5,10),
      'C' => (7,100),
      'D' => (9,1000),
      _ => continue,
    };
    let mut cost =
      if y > room && (room..y).all(|c| maze[1][c] == '.') {
        y - room
      } else if y < room && (y+1..=room).all(|c| maze[1][c] == '.') {
        room - y
      } else {
        continue
      };
    let i = match (2..=room_len).take_while(|&i| maze[i][room] == '.').last() {
      Some(i) => i,
      _ => continue
    };
    if i != room_len && maze[i+1][room] != maze[1][y] { continue; }
    let mut m = maze.clone();
    m[i][room] = maze[1][y];
    m[1][y] = '.';
    cost += i-1;
    moves.push(((cost * exp) as i64,m));
  }
  for (x,y) in (2..=room_len).cartesian_product([3,5,7,9]) {
    // check moving out of a room
    if (2..x).any(|i| maze[i][y] != '.') {
      continue;
    }
    if (x+1..=room_len).any(|i| maze[i][y] == '.') {
      continue;
    }
    let exp = match maze[x][y] {
      'A' => 1,
      'B' => 10,
      'C' => 100,
      'D' => 1000,
      _ => continue,
    };
    for i in y..maze[0].len() { // move left
      if maze[1][i] != '.' { break; }
      if ![1,2,4,6,8,10,11].contains(&i) { continue; }
      let cost = x - 1 + i - y;
      let mut m = maze.clone();
      m[1][i] = maze[x][y];
      m[x][y] = '.';
      moves.push(((cost*exp) as i64,m));
    }
    for i in (1..=y).rev() { // move right
      if maze[1][i] != '.' { break; }
      if ![1,2,4,6,8,10,11].contains(&i) { continue; }
      let cost = x - 1 + y - i;
      let mut m = maze.clone();
      m[1][i] = maze[x][y];
      m[x][y] = '.';
      moves.push(((cost*exp) as i64,m));
    }
  }
  moves
}

fn shortest_path(maze: &Vec<Vec<char>>) -> i64 {
  let mut dist = HashMap::new();
  let mut q = BinaryHeap::new();
  q.push((0,maze.clone()));
  while let Some((cost,m)) = q.pop() {
    if right_configuration(&m) { return -cost; }
    if let Some(&c) = dist.get(&m) {
      if -cost > c { continue; }
    }
    for (nmoves, m) in moves(&m) {
      let next_cost = -cost + nmoves;
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
  let mut map = INPUT.lines().map(|l| l.chars().collect()).collect();
  let p1 = shortest_path(&map);
  map.insert(4, "  #D#C#B#A#  ".chars().collect());
  map.insert(4, "  #D#B#A#C#  ".chars().collect());
  let p2 = shortest_path(&map);
  (p1,p2)
}
