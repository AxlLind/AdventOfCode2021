use hashbrown::HashSet;

fn fold_grid(points: &HashSet<(i32,i32)>, (dir,pos): (char,i32)) -> HashSet<(i32,i32)> {
  points.iter().map(|&(x,y)| match (dir,x,y) {
    ('x',x,y) if x < pos => (x,y),
    ('y',x,y) if y < pos => (x,y),
    ('x',x,y) => (pos*2 - x,y),
    ('y',x,y) => (x,pos*2 - y),
    _ => unreachable!()
  }).collect()
}

fn grid_to_string(grid: &HashSet<(i32,i32)>) -> String {
  let xmax = grid.iter().map(|&(x,_)| x).max().unwrap();
  let ymax = grid.iter().map(|&(_,y)| y).max().unwrap();
  let mut s = "\n".to_string();
  for y in 0..=ymax {
    for x in 0..=xmax {
      s += if grid.contains(&(x,y)) {"█"} else {" "};
    }
    s += "\n";
  }
  s
}

#[aoc::main("13")]
fn main(input: &str) -> (usize,String) {
  let (points, folds) = input.split_once("\n\n").unwrap();
  let grid = points.lines()
    .map(|l| {
      let (a,b) = l.split_once(',').unwrap();
      (a.parse().unwrap(), b.parse().unwrap())
    })
    .collect();
  let folds = folds.lines()
    .map(|s| (s.as_bytes()[11] as char, s[13..].parse().unwrap()))
    .collect::<Vec<_>>();
  let p1 = fold_grid(&grid, folds[0]).len();
  let final_grid = folds.iter().fold(grid, |grid, &fold| fold_grid(&grid, fold));
  (p1, grid_to_string(&final_grid))
}
