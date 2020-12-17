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
  (-1, -1,-1,-1),
  (-1, -1,-1, 0),
  (-1, -1,-1, 1),
  (-1, -1, 0,-1),
  (-1, -1, 0, 0),
  (-1, -1, 0, 1),
  (-1, -1, 1,-1),
  (-1, -1, 1, 0),
  (-1, -1, 1, 1),
  (-1,  0,-1,-1),
  (-1,  0,-1, 0),
  (-1,  0,-1, 1),
  (-1,  0, 0,-1),
  (-1,  0, 0, 0),
  (-1,  0, 0, 1),
  (-1,  0, 1,-1),
  (-1,  0, 1, 0),
  (-1,  0, 1, 1),
  (-1,  1,-1,-1),
  (-1,  1,-1, 0),
  (-1,  1,-1, 1),
  (-1,  1, 0,-1),
  (-1,  1, 0, 0),
  (-1,  1, 0, 1),
  (-1,  1, 1,-1),
  (-1,  1, 1, 0),
  (-1,  1, 1, 1),
  ( 0, -1,-1,-1),
  ( 0, -1,-1, 0),
  ( 0, -1,-1, 1),
  ( 0, -1, 0,-1),
  ( 0, -1, 0, 0),
  ( 0, -1, 0, 1),
  ( 0, -1, 1,-1),
  ( 0, -1, 1, 0),
  ( 0, -1, 1, 1),
  ( 0,  0,-1,-1),
  ( 0,  0,-1, 0),
  ( 0,  0,-1, 1),
  ( 0,  0, 0,-1),
  ( 0,  0, 0, 1),
  ( 0,  0, 1,-1),
  ( 0,  0, 1, 0),
  ( 0,  0, 1, 1),
  ( 0,  1,-1,-1),
  ( 0,  1,-1, 0),
  ( 0,  1,-1, 1),
  ( 0,  1, 0,-1),
  ( 0,  1, 0, 0),
  ( 0,  1, 0, 1),
  ( 0,  1, 1,-1),
  ( 0,  1, 1, 0),
  ( 0,  1, 1, 1),
  ( 1, -1,-1,-1),
  ( 1, -1,-1, 0),
  ( 1, -1,-1, 1),
  ( 1, -1, 0,-1),
  ( 1, -1, 0, 0),
  ( 1, -1, 0, 1),
  ( 1, -1, 1,-1),
  ( 1, -1, 1, 0),
  ( 1, -1, 1, 1),
  ( 1,  0,-1,-1),
  ( 1,  0,-1, 0),
  ( 1,  0,-1, 1),
  ( 1,  0, 0,-1),
  ( 1,  0, 0, 0),
  ( 1,  0, 0, 1),
  ( 1,  0, 1,-1),
  ( 1,  0, 1, 0),
  ( 1,  0, 1, 1),
  ( 1,  1,-1,-1),
  ( 1,  1,-1, 0),
  ( 1,  1,-1, 1),
  ( 1,  1, 0,-1),
  ( 1,  1, 0, 0),
  ( 1,  1, 0, 1),
  ( 1,  1, 1,-1),
  ( 1,  1, 1, 0),
  ( 1,  1, 1, 1),
];

fn simulate<P: Hash + Eq + Copy, F: Fn(&HashSet<P>) -> HashMap<P,usize>>(
  active: HashSet<P>,
  count_neighbours: F,
) -> usize {
  (0..6).fold(active, |active, _|
    count_neighbours(&active).iter()
      .filter(|(pos,n)| matches!(
        (active.contains(pos),n),
        (true,2) | (true,3) | (false,3)
      ))
      .map(|(&pos,_)| pos)
      .collect()
  ).len()
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

fn part_one() -> usize {
  let active = INPUT.iter()
    .enumerate()
    .flat_map(|(y, row)| row.chars()
      .enumerate()
      .filter(|&(_,b)| b == '#')
      .map(move |(x,_)| (x as i8, y as i8, 0))
    )
    .collect();
  simulate(active, count_neighbours_p1)
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

fn part_two() -> usize {
  let active = INPUT.iter()
    .enumerate()
    .flat_map(|(y, row)| row.chars()
      .enumerate()
      .filter(|&(_,b)| b == '#')
      .map(move |(x,_)| (x as i8, y as i8, 0, 0))
    )
    .collect();
  simulate(active, count_neighbours_p2)
}


aoc2020::main! {
  (part_one(), part_two())
}
