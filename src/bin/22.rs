use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn disintegrate_all(adjacent: &[(HashSet<usize>, HashSet<usize>)], falling: &mut HashSet<usize>, i: usize) {
  if !falling.insert(i) {
    return;
  }
  for &above in &adjacent[i].0 {
    if adjacent[above].1.iter().all(|x| falling.contains(x)) {
      disintegrate_all(adjacent, falling, above);
    }
  }
}

#[aoc::main(22)]
fn main(input: &str) -> (usize, usize) {
  let mut bricks = input.split('\n').map(|l| {
    let (x1, y1, z1, x2, y2, z2) = l.split(|c: char| !c.is_ascii_digit())
      .map(|w| w.parse::<usize>().unwrap())
      .collect_tuple()
      .unwrap();
    (x1, y1, z1, x2, y2, z2)
  }).collect::<Vec<_>>();
  bricks.sort_by_key(|b| b.2);
  let mut space = HashMap::new();
  for (i, b) in bricks.iter_mut().enumerate() {
    let (x1, y1, mut z1, x2, y2, mut z2) = *b;
    while z1 > 1 && (x1..=x2).cartesian_product(y1..=y2).all(|(x,y)| !space.contains_key(&(x,y,z1-1))) {
      z2 -= 1;
      z1 -= 1;
    }
    let positions = (x1..=x2).cartesian_product(y1..=y2).cartesian_product(z1..=z2);
    space.extend(positions.map(|((x,y),z)| ((x,y,z),i)));
    *b = (x1, y1, z1, x2, y2, z2);
  }
  let mut adjacent = vec![(HashSet::new(), HashSet::new()); bricks.len()];
  for (i, &(x1, y1, z1, x2, y2, _)) in bricks.iter().enumerate() {
    for (x,y) in (x1..=x2).cartesian_product(y1..=y2) {
      if let Some(&j) = space.get(&(x,y,z1-1)) {
        adjacent[j].0.insert(i);
        adjacent[i].1.insert(j);
      }
    }
  }
  let (mut p1, mut p2) = (0, 0);
  let mut falling = HashSet::new();
  for b in 0..bricks.len() {
    falling.clear();
    disintegrate_all(&adjacent, &mut falling, b);
    p1 += (falling.len() == 1) as usize;
    p2 += falling.len() - 1;
  }
  (p1, p2)
}
