use hashbrown::HashSet;
use itertools::Itertools;

fn sides((x,y,z): (i32,i32,i32)) -> [(i32,i32,i32); 6] {
  [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
}

#[aoc::main(18)]
fn main(input: &str) -> (usize, usize) {
  let drops = input.lines()
    .filter_map(|l| l.split(',').map(|x| x.parse().unwrap()).collect_tuple())
    .collect::<HashSet<_>>();
  let max = drops.iter().flat_map(|&(x,y,z)| [x,y,z]).max().unwrap() + 1;
  let (mut seen, mut stack) = (HashSet::new(), vec![(0,0,0)]);
  while let Some(p) = stack.pop() {
    for (x,y,z) in sides(p) {
      if !drops.contains(&(x,y,z)) && !seen.contains(&(x,y,z)) && [x,y,z].iter().all(|&i| -1 <= i && i <= max) {
        seen.insert((x,y,z));
        stack.push((x,y,z));
      }
    }
  }
  let p1 = drops.iter().flat_map(|&p| sides(p)).filter(|s| !drops.contains(s)).count();
  let p2 = drops.iter().flat_map(|&p| sides(p)).filter(|s| seen.contains(s)).count();
  (p1, p2)
}
