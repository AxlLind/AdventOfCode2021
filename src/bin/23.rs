use hashbrown::HashMap;
use std::collections::BinaryHeap;

static INPUT: [[u8;2];4] = [[b'C',b'D'], [b'A',b'C'], [b'B',b'A'], [b'D',b'B']];
static INPUT2: [[u8;4];4] = [[b'C',b'D',b'D',b'D'], [b'A',b'C',b'B',b'C'], [b'B',b'B',b'A',b'A'], [b'D',b'A',b'C',b'B']];

type State<const N: usize> = ([u8;11], [[u8;N];4]);

fn right_configuration<const N: usize>((_,rooms): &State<N>) -> bool {
  for (room, c) in rooms.iter().zip("ABCD".bytes()) {
    if room.iter().any(|&x| x != c) {
      return false;
    }
  }
  true
}

fn make_move<const N: usize>((mut corridor, mut rooms): State<N>, cost: usize, c: usize, room: usize, depth: usize) -> (usize, State<N>) {
  let c0 = [2,4,6,8][room];
  let energy = (depth + if c0 > c {c0-c} else {c-c0} + 1) * cost;
  std::mem::swap(&mut corridor[c], &mut rooms[room][depth]);
  (energy, (corridor, rooms))
}

fn moves<const N: usize>((corridor,rooms): State<N>) -> Vec<(usize,State<N>)> {
  let mut moves = Vec::new();
  for c in 0..corridor.len() { // check moving into a room
    let (room, cost) = match corridor[c] {
      b'A' => (0,1),
      b'B' => (1,10),
      b'C' => (2,100),
      b'D' => (3,1000),
      _ => continue,
    };
    let c0 = [2,4,6,8][room];
    let (r0,r1) = if c > c0 {(c0,c)} else {(c+1,c0+1)};
    if (r0..r1).any(|i| corridor[i] != b'.') { continue; }
    let i = match (0..N).take_while(|&i| rooms[room][i] == b'.').last() {
      Some(i) => i,
      _ => continue
    };
    if i+1 != N && rooms[room][i+1] != corridor[c] { continue; }
    moves.push(make_move((corridor, rooms), cost, c, room, i));
  }
  for room in 0..4 { // check moving out of a room
    let i = match (0..N).find(|&i| rooms[room][i] != b'.') {
      Some(i) => i,
      _ => continue
    };
    let cost = match rooms[room][i] {
      b'A' => 1,
      b'B' => 10,
      b'C' => 100,
      b'D' => 1000,
      _ => continue,
    };
    let c0 = [2,4,6,8][room];
    for c in c0..corridor.len() { // move right
      if corridor[c] != b'.' { break; }
      if ![2,4,6,8].contains(&c) {
        moves.push(make_move((corridor, rooms), cost, c, room, i));
      }
    }
    for c in (0..c0).rev() { // move left
      if corridor[c] != b'.' { break; }
      if ![2,4,6,8].contains(&c) {
        moves.push(make_move((corridor, rooms), cost, c, room, i));
      }
    }
  }
  moves
}

fn shortest_path<const N: usize>(state: State<N>) -> i64 {
  let mut dist = HashMap::new();
  let mut q = BinaryHeap::new();
  q.push((0,state));
  while let Some((cost,m)) = q.pop() {
    if right_configuration(&m) { return -cost; }
    if let Some(&c) = dist.get(&m) {
      if -cost > c { continue; }
    }
    for (energy, m) in moves(m) {
      let next_cost = -cost + energy as i64;
      let &c = dist.get(&m).unwrap_or(&1000000);
      if c > next_cost {
        dist.insert(m, next_cost);
        q.push((-next_cost,m));
      }
    }
  }
  unreachable!()
}

aoc2021::main! {
  let p1 = shortest_path(([b'.';11], INPUT));
  let p2 = shortest_path(([b'.';11], INPUT2));
  (p1,p2)
}
