use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn disintegrate_all(
  bricks: &[(usize, usize, usize, usize, usize, usize, usize)],
  falling: &mut HashSet<usize>,
  above: &HashMap<usize, HashSet<usize>>,
  below: &HashMap<usize, HashSet<usize>>,
  brick: usize
) {
  if !falling.insert(brick) {
    return;
  }
  let Some(parents) = above.get(&brick) else { return };
  for &parent in parents {
    if below[&parent].iter().all(|x| falling.contains(x)) {
      disintegrate_all(bricks, falling, above, below, parent);
    }
  }
}

#[aoc::main(22)]
fn main(input: &str) -> (usize, usize) {
  let mut bricks = input.split('\n').enumerate().map(|(i,l)| {
    let (a, b) = l.split_once('~').unwrap();
    let (x1,y1,z1) = a.split(',').map(|w| w.parse().unwrap()).collect_tuple().unwrap();
    let (x2,y2,z2) = b.split(',').map(|w| w.parse().unwrap()).collect_tuple().unwrap();
    (x1, y1, z1, x2, y2, z2, i)
  }).collect::<Vec<_>>();
  let mut grid = HashMap::new();
  for &(x1, y1, z1, x2, y2, z2, i) in &bricks {
    for x in x1..=x2 {
      for y in y1..=y2 {
        for z in z1..=z2 {
          grid.insert((x,y,z), i);
        }
      }
    }
  }
  let mut done = false;
  while !done {
    done = true;
    for b in &mut bricks {
      loop {
        let (x1, y1, z1, x2, y2, z2, i) = *b;
        if z1 == 1 {
          break;
        }
        if (x1..=x2).cartesian_product(y1..=y2).any(|(x,y)| grid.contains_key(&(x,y,z1-1))) {
          break;
        }
        *b = (x1, y1, z1-1, x2, y2, z2-1, i);
        for (x,y) in (x1..=x2).cartesian_product(y1..=y2) {
          grid.remove(&(x,y,z2));
          grid.insert((x,y,z1-1), i);
        }
        done = false;
      }
    }
  }
  let mut above = HashMap::<_,HashSet<_>>::new();
  let mut below = HashMap::<_,HashSet<_>>::new();
  for &(x1, y1, z1, x2, y2, _, i) in &bricks {
    for (x,y) in (x1..=x2).cartesian_product(y1..=y2) {
      if let Some(&j) = grid.get(&(x,y,z1-1)) {
        above.entry(j).or_default().insert(i);
        below.entry(i).or_default().insert(j);
      }
    }
  }
  let (mut p1, mut p2) = (0, 0);
  for b in 0..bricks.len() {
    let mut falling = HashSet::new();
    disintegrate_all(&bricks, &mut falling, &above, &below, b);
    p1 += if falling.len() == 1 {1} else {0};
    p2 += falling.len() - 1;
  }
  (p1, p2)
}
