use std::collections::*;
use std::hash::Hash;

static INPUT: [&str; 8] = [
  "#####...",
  ".#..##..",
  "##.##.##",
  "...####.",
  "#.#...##",
  ".##...#.",
  ".#.#.###",
  "#.#.#..#",
];

static NEIGHBOURS: [(i8,i8,i8,i8); 80] = [
  (-1,-1,-1,-1), (-1,-1,-1, 0), (-1,-1,-1, 1),
  (-1,-1, 0,-1), (-1,-1, 0, 0), (-1,-1, 0, 1),
  (-1,-1, 1,-1), (-1,-1, 1, 0), (-1,-1, 1, 1),
  (-1, 0,-1,-1), (-1, 0,-1, 0), (-1, 0,-1, 1),
  (-1, 0, 0,-1), (-1, 0, 0, 0), (-1, 0, 0, 1),
  (-1, 0, 1,-1), (-1, 0, 1, 0), (-1, 0, 1, 1),
  (-1, 1,-1,-1), (-1, 1,-1, 0), (-1, 1,-1, 1),
  (-1, 1, 0,-1), (-1, 1, 0, 0), (-1, 1, 0, 1),
  (-1, 1, 1,-1), (-1, 1, 1, 0), (-1, 1, 1, 1),
  ( 0,-1,-1,-1), ( 0,-1,-1, 0), ( 0,-1,-1, 1),
  ( 0,-1, 0,-1), ( 0,-1, 0, 0), ( 0,-1, 0, 1),
  ( 0,-1, 1,-1), ( 0,-1, 1, 0), ( 0,-1, 1, 1),
  ( 0, 0,-1,-1), ( 0, 0,-1, 0), ( 0, 0,-1, 1),
  ( 0, 0, 0,-1),                ( 0, 0, 0, 1),
  ( 0, 0, 1,-1), ( 0, 0, 1, 0), ( 0, 0, 1, 1),
  ( 0, 1,-1,-1), ( 0, 1,-1, 0), ( 0, 1,-1, 1),
  ( 0, 1, 0,-1), ( 0, 1, 0, 0), ( 0, 1, 0, 1),
  ( 0, 1, 1,-1), ( 0, 1, 1, 0), ( 0, 1, 1, 1),
  ( 1,-1,-1,-1), ( 1,-1,-1, 0), ( 1,-1,-1, 1),
  ( 1,-1, 0,-1), ( 1,-1, 0, 0), ( 1,-1, 0, 1),
  ( 1,-1, 1,-1), ( 1,-1, 1, 0), ( 1,-1, 1, 1),
  ( 1, 0,-1,-1), ( 1, 0,-1, 0), ( 1, 0,-1, 1),
  ( 1, 0, 0,-1), ( 1, 0, 0, 0), ( 1, 0, 0, 1),
  ( 1, 0, 1,-1), ( 1, 0, 1, 0), ( 1, 0, 1, 1),
  ( 1, 1,-1,-1), ( 1, 1,-1, 0), ( 1, 1,-1, 1),
  ( 1, 1, 0,-1), ( 1, 1, 0, 0), ( 1, 1, 0, 1),
  ( 1, 1, 1,-1), ( 1, 1, 1, 0), ( 1, 1, 1, 1),
];

fn simulate<Pos: Hash + Eq + Copy, F: Fn(&HashSet<Pos>) -> HashMap<Pos,usize>>(
  mut active: HashSet<Pos>,
  count_neighbours: F,
) -> usize {
  for _ in 0..6 {
    active = count_neighbours(&active).iter()
      .filter(|&(pos,&n)| n == 3 || (n == 2 && active.contains(pos)))
      .map(|(&pos,_)| pos)
      .collect();
  }
  active.len()
}

fn count_neighbours_p1(active: &HashSet<(i8,i8,i8)>) -> HashMap<(i8,i8,i8), usize> {
  let mut neighbours = HashMap::new();
  for (x,y,z) in active {
    for (_,dx,dy,dz) in &NEIGHBOURS[26..52] {
      *neighbours.entry((x+dx, y+dy, z+dz)).or_insert(0) += 1;
    }
  }
  neighbours
}

fn count_neighbours_p2(active: &HashSet<(i8,i8,i8,i8)>) -> HashMap<(i8,i8,i8,i8), usize> {
  let mut neighbours = HashMap::new();
  for (x,y,z,w) in active {
    for (dx,dy,dz,dw) in &NEIGHBOURS {
      *neighbours.entry((x+dx, y+dy, z+dz, w+dw)).or_insert(0) += 1;
    }
  }
  neighbours
}

aoc2020::main! {
  let active1 = INPUT.iter()
    .enumerate()
    .flat_map(|(y, row)| row.chars()
      .enumerate()
      .filter(|&(_,b)| b == '#')
      .map(move |(x,_)| (x as i8, y as i8, 0))
    )
    .collect::<HashSet<_>>();
  let active2 = active1.iter()
    .map(|&(x,y,z)| (x,y,z,0))
    .collect();
  let p1 = simulate(active1, count_neighbours_p1);
  let p2 = simulate(active2, count_neighbours_p2);
  (p1, p2)
}
