use std::ops::RangeInclusive;
use itertools::Itertools;

static TARGETX: RangeInclusive<i32> = 25..=67;
static TARGETY: RangeInclusive<i32> = -260..=-200;

fn try_vel(mut dx: i32, mut dy: i32) -> Option<i32> {
  let (mut x, mut y, mut maxy) = (0,0,0);
  loop {
    x += dx;
    y += dy;
    dx -= dx.signum();
    dy -= 1;
    if y > maxy { maxy = y; }
    match (TARGETX.contains(&x), TARGETY.contains(&y)) {
      (true,true) => return Some(maxy),
      (false,_) if dx == 0 => return None,
      (_,false) if dy < 0 && y < -260 => return None,
      _ => {}
    }
  }
}

aoc2021::main! {
  let maxys = (0..100).cartesian_product(-260..1000)
    .filter_map(|(x,y)| try_vel(x,y))
    .collect::<Vec<_>>();
  (*maxys.iter().max().unwrap(), maxys.len())
}
