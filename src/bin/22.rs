use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn disintegrate_all(
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
      disintegrate_all(falling, above, below, parent);
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
  let mut grid = HashMap::new();
  for (i, b) in bricks.iter_mut().enumerate() {
    let (x1, y1, mut z1, x2, y2, mut z2) = *b;
    while z1 > 1 && (x1..=x2).cartesian_product(y1..=y2).all(|(x,y)| !grid.contains_key(&(x,y,z1-1))) {
      z2 -= 1;
      z1 -= 1;
    }
    for x in x1..=x2 {
      for y in y1..=y2 {
        for z in z1..=z2 {
          grid.insert((x,y,z), i);
        }
      }
    }
    *b = (x1, y1, z1, x2, y2, z2);
  }
  let mut above = HashMap::<_,HashSet<_>>::new();
  let mut below = HashMap::<_,HashSet<_>>::new();
  for (i, &(x1, y1, z1, x2, y2, _)) in bricks.iter().enumerate() {
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
    disintegrate_all(&mut falling, &above, &below, b);
    p1 += (falling.len() == 1) as usize;
    p2 += falling.len() - 1;
  }
  (p1, p2)
}
