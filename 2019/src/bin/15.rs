use std::time::Instant;
use std::collections::{HashMap, HashSet, VecDeque};
use intcoder::{IntCoder, ExitCode};
use itertools::Itertools;

const PROGRAM: [i64; 1045] = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,101,0,1034,1039,102,1,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1106,0,124,1001,1034,-1,1039,1008,1036,0,1041,1002,1035,1,1040,1001,1038,0,1043,101,0,1037,1042,1106,0,124,1001,1034,1,1039,1008,1036,0,1041,101,0,1035,1040,102,1,1038,1043,1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,35,1032,1006,1032,165,1008,1040,9,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1105,1,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,26,1044,1105,1,224,1101,0,0,1044,1106,0,224,1006,1044,247,102,1,1039,1034,101,0,1040,1035,102,1,1041,1036,1002,1043,1,1038,1001,1042,0,1037,4,1044,1106,0,0,22,11,19,72,14,9,6,73,82,17,41,18,83,18,49,19,12,14,39,17,20,69,20,12,48,8,8,59,36,7,33,1,15,13,10,46,96,15,2,22,80,99,12,68,99,79,22,84,16,45,25,51,4,20,95,4,51,43,13,89,2,91,48,2,46,55,24,84,8,88,10,98,46,57,15,27,7,1,19,20,63,24,50,13,63,13,59,19,13,53,75,8,20,8,44,44,21,5,11,76,9,21,2,11,27,61,6,12,72,22,40,11,9,50,18,2,38,21,78,18,13,99,9,74,5,22,30,35,5,16,34,91,55,4,19,28,42,21,62,12,74,94,16,40,2,95,54,21,2,23,56,34,9,49,47,14,39,9,65,35,53,23,25,68,15,95,25,70,27,3,33,2,31,17,40,60,24,94,34,6,99,9,92,1,92,7,49,32,8,46,47,13,37,15,11,2,15,24,8,73,8,21,64,19,74,24,5,60,9,21,47,12,12,72,18,39,90,16,6,85,13,71,19,14,24,2,65,11,51,9,19,23,34,12,9,88,77,17,6,72,19,79,39,19,21,95,87,24,91,53,7,29,20,25,11,39,38,24,72,6,1,97,15,87,11,77,64,17,57,95,9,85,19,77,8,18,97,8,39,49,4,16,81,12,36,7,7,81,22,52,56,22,47,42,4,46,75,21,19,85,37,22,90,20,10,56,24,85,55,4,91,7,22,86,1,89,13,68,35,14,27,35,9,44,79,12,42,20,16,28,89,11,57,10,60,15,13,95,3,48,24,90,86,51,18,8,71,11,80,91,5,4,93,9,80,94,9,31,7,6,90,6,57,18,19,41,69,57,8,3,42,21,16,5,79,9,13,56,99,98,19,22,85,14,35,12,21,69,16,23,3,5,78,68,2,24,12,35,36,24,93,72,12,16,7,7,19,56,8,69,45,94,18,49,44,61,21,25,19,96,7,13,27,50,76,14,5,60,4,11,90,60,9,31,85,17,11,18,74,37,20,53,53,1,42,93,66,24,10,10,73,36,19,84,14,87,71,18,64,58,3,9,70,14,10,62,81,25,19,52,5,3,78,10,66,84,84,14,66,9,19,81,8,56,11,7,39,84,31,98,22,25,56,4,12,43,78,20,19,43,88,23,10,62,90,22,38,29,5,29,32,20,14,1,3,44,13,92,79,11,59,22,77,38,3,83,18,22,37,24,32,8,19,47,20,23,32,14,72,80,24,37,33,20,8,12,17,31,20,13,51,68,65,19,31,1,1,47,88,15,31,25,94,4,11,95,87,16,77,86,92,3,2,48,39,52,62,22,63,1,70,18,61,78,14,12,50,75,10,30,2,10,96,13,58,87,9,90,3,83,5,13,28,3,67,66,21,46,10,1,70,64,8,10,50,13,22,93,3,58,13,58,2,69,1,44,2,18,22,61,61,25,36,20,7,31,6,2,7,29,2,27,22,93,16,25,8,79,93,22,2,29,27,12,56,48,34,6,40,14,13,8,14,2,8,64,32,19,18,99,22,83,83,79,16,84,58,22,88,19,31,18,35,18,31,85,20,30,16,75,16,46,16,65,16,3,44,6,2,65,97,24,40,20,25,31,88,14,66,20,13,11,76,18,43,67,13,92,47,9,81,78,20,51,12,7,43,17,24,99,14,4,89,13,84,48,13,60,13,51,23,66,7,61,19,91,17,72,64,48,10,74,13,85,8,76,11,72,3,32,22,37,80,44,18,86,50,71,5,36,21,76,23,64,23,61,40,62,24,61,0,0,21,21,1,10,1,0,0,0,0,0,0];

type Pos = (i64,i64);

fn print_map(map: &HashMap<Pos,i64>) {
  let (xmin,ymin,xmax,ymax) = map.keys()
    .fold((0,0,0,0), |(xmin,ymin,xmax,ymax), &(x,y)| {
      let xmin = std::cmp::min(x, xmin);
      let ymin = std::cmp::min(y, ymin);
      let xmax = std::cmp::max(x, xmax);
      let ymax = std::cmp::max(y, ymax);
      (xmin,ymin,xmax,ymax)
    });
  for y in ymin..=ymax {
    for x in xmin..=xmax {
      let c = match map.get(&(x,y)) {
        Some(&1) => ' ',
        Some(&2) => '$',
        _        => '█'
      };
      print!("{}", if x == 0 && y == 0 {'#'} else {c});
    }
    println!();
  }
}

fn compute_paths(map: &HashMap<Pos, i64>, start: Pos) -> (HashMap<Pos, i64>, HashMap<Pos, Option<Pos>>) {
  let mut q = map.iter()
    .filter(|&(_, &v)| v == 1)
    .map(|(&pos,_)| pos)
    .collect::<HashSet<_>>();
  let mut dist = q.iter().map(|&pos| (pos, 9999)).collect::<HashMap<_,_>>();
  let mut prev = q.iter().map(|&pos| (pos, None)).collect::<HashMap<_,_>>();
  dist.insert(start, 0);

  let neighbors = [(0,-1), (0,1), (-1,0), (1,0)];
  while !q.is_empty() {
    let (ux,uy) = *q.iter().min_by_key(|pos| dist[pos]).unwrap();
    q.remove(&(ux,uy));

    neighbors.iter()
      .map(|(dx,dy)| (ux + dx, uy + dy))
      .filter(|v| q.contains(v))
      .for_each(|v| {
        let d = dist[&(ux,uy)] + 1;
        if d < dist[&v] {
          dist.insert(v, d);
          prev.insert(v, Some((ux,uy)));
        }
      });
  };
  (dist, prev)
}

fn compute_answers(map: &HashMap<Pos, i64>, (x,y): Pos) -> Pos {
  let (dist,prev) = compute_paths(map, (x, y));

  let mut curr = (0,0);
  let mut part_one = 0;
  loop {
    match prev[&curr] {
      Some(val) => curr = val,
      None => break,
    }
    part_one += 1
  }

  let part_two = dist.values().max().unwrap();
  (part_one + 1, part_two + 1)
}

fn find_path_to_unexplored(map: &HashMap<Pos, i64>, start: Pos) -> Option<VecDeque<Pos>> {
  let mut queue = VecDeque::new();
  let mut visited = HashSet::new();
  queue.push_back(vec![start]);
  visited.insert(start);

  let neighbors = [(0,-1), (0,1), (-1,0), (1,0)];
  let path = loop {
    if queue.is_empty() { return None; }
    let path = queue.pop_front().unwrap();
    let (ux, uy) = *path.last().unwrap();

    visited.insert((ux,uy));
    if !map.contains_key(&(ux, uy)) { break path; }

    neighbors.iter()
      .map(|(dx,dy)| (ux + dx, uy + dy))
      .filter(|pos| match map.get(pos) {
        Some(&val) => val != 0 && !visited.contains(pos),
        None       => true,
      })
      .for_each(|pos| {
        let mut new_path = path.clone();
        new_path.push(pos);
        queue.push_back(new_path);
      });
  };

  let answer = path.iter()
    .tuples()
    .map(|((x1,y1),(x2,y2))| (x2-x1, y2-y1))
    .collect();
  Some(answer)
}

fn explore_map() -> (HashMap<Pos,i64>, Pos) {
  let mut cpu = IntCoder::new(&PROGRAM);
  let mut map = HashMap::new();

  let mut prev_input = (0,-1);
  let mut inputs = VecDeque::from(vec![prev_input]);

  let mut goal = (0,0);
  let (mut x, mut y) = (0,0);
  loop {
    match cpu.execute() {
      ExitCode::Output(v) => {
        let (dx,dy) = prev_input;
        map.insert((x+dx, y+dy), v);
        if v == 0 {
          inputs.clear();
          continue;
        }
        if v == 2 { goal = (x,y); }
        x += dx;
        y += dy;
      }
      ExitCode::AwaitInput => {
        if inputs.is_empty() {
          match find_path_to_unexplored(&map, (x, y)) {
            Some(p) => inputs = p,
            None    => break,
          }
        }
        prev_input = inputs.pop_front().unwrap();
        cpu.push_input(match prev_input {
          ( 0,-1) => 1,
          ( 0, 1) => 2,
          (-1, 0) => 3,
          ( 1, 0) => 4,
          _ => unreachable!(),
        });
      },
      ExitCode::Halted => unreachable!(),
    }
  }
  (map, goal)
}

fn main() {
  let now = Instant::now();
  let (map, goal) = explore_map();
  let (part_one, part_two) = compute_answers(&map, goal);
  print_map(&map);
  println!("Part one: {}", part_one);
  println!("Part two: {}", part_two);
  println!("Time: {}ms", now.elapsed().as_millis());
}
