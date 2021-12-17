use itertools::Itertools;

const XMIN: i32 = 25;
const XMAX: i32 = 67;
const YMIN: i32 = -260;
const YMAX: i32 = -200;

fn try_vel(mut dx: i32, mut dy: i32) -> Option<i32> {
  let (mut x, mut y, mut maxy) = (0,0,0);
  loop {
    x += dx;
    y += dy;
    dx -= dx.signum();
    dy -= 1;
    if y > maxy { maxy = y; }
    match (XMIN <= x && x <= XMAX, YMIN <= y && y <= YMAX) {
      (true,true) => return Some(maxy),
      (false,_) if dx == 0 => return None,
      (_,false) if dy < 0 && y < YMIN => return None,
      _ => {}
    }
  }
}

aoc2021::main! {
  let maxys = (0..=XMAX).cartesian_product(YMIN..1000)
    .filter_map(|(x,y)| try_vel(x,y))
    .collect::<Vec<_>>();
  (*maxys.iter().max().unwrap(), maxys.len())
}
