use itertools::Itertools;

fn simulate(mut map: Vec<Vec<bool>>, floor: usize, breakpoint: usize) -> usize {
  for ans in 0.. {
    let (mut x, mut y) = (500, 0);
    while y + 1 != floor {
      if !map[x][y+1] {
        y += 1;
      } else if !map[x-1][y+1] {
        x -= 1;
        y += 1;
      } else if !map[x+1][y+1] {
        x += 1;
        y += 1;
      } else {
        break;
      }
    }
    if y == breakpoint { return ans; }
    map[x][y] = true;
  }
  unreachable!()
}

#[aoc::main(14)]
fn main(input: &str) -> (usize, usize) {
  let mut map = vec![vec![false; 1000]; 1000];
  let mut max_y = 0;
  for l in input.lines() {
    let coords = l.split(" -> ").map(|x| {
      let (a,b) = x.split_once(",").unwrap();
      (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap())
    });
    for ((x1,y1),(x2,y2)) in coords.tuple_windows() {
      max_y = std::cmp::max(max_y, std::cmp::max(y1, y2));
      let (mut x1,mut y1,x2,y2) = (x1 as isize,y1 as isize,x2 as isize,y2 as isize);
      let dx = (x2 - x1).signum();
      let dy = (y2 - y1).signum();
      map[x1 as usize][y1 as usize] = true;
      while (x1,y1) != (x2,y2) {
        x1 += dx;
        y1 += dy;
        map[x1 as usize][y1 as usize] = true;
      }
    }
  }
  let p1 = simulate(map.clone(), max_y + 2, max_y + 1);
  let p2 = simulate(map, max_y + 2, 0) + 1;
  (p1,p2)
}
