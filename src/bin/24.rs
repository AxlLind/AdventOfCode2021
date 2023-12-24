use itertools::Itertools;

fn intersects((x1,y1,dx1,dy1): (f64,f64,f64,f64), (x2,y2,dx2,dy2): (f64,f64,f64,f64)) -> Option<(f64, f64)> {
  let m1 = dy1 / dx1;
  let m2 = dy2 / dx2;
  if (m2 - m1).abs() < f64::EPSILON {
    return None;
  }
  let x = (m1 * x1 - m2*x2 + y2 - y1) / (m1 - m2);
  let y = (m1*m2*(x2-x1) + m2*y1 - m1*y2) / (m2 - m1);
  Some((x,y))
}

fn find_intersections(lines: &[((f64,f64,f64),(f64,f64,f64))], start: f64, end: f64) -> usize {
  let mut intersections = 0;
  for (&((x1,y1,_),(dx1,dy1,_)),&((x2,y2,_),(dx2,dy2,_))) in lines.iter().tuple_combinations() {
    if let Some((x,y)) = intersects((x1,y1,dx1,dy1), (x2,y2,dx2,dy2)) {
      if (dx1 < 0.0 && x > x1) || (dx1 > 0.0 && x < x1){
        continue;
      }
      if (dx2 < 0.0 && x > x2) || (dx2 > 0.0 && x < x2){
        continue;
      }
      if (start..=end).contains(&x) && (start..=end).contains(&y) {
        intersections += 1;
      }
    }
  }
  intersections
}

#[aoc::main(24)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(|l| {
    let (a,b) = l.split_once(" @ ").unwrap();
    let (x,y,z) = a.split(", ").map(|w| w.parse::<f64>().unwrap()).collect_tuple().unwrap();
    let (dx,dy,dz) = b.split(", ").map(|w| w.trim().parse::<f64>().unwrap()).collect_tuple().unwrap();
    ((x,y,z), (dx,dy,dz))
  }).collect::<Vec<_>>();
  let p1 = find_intersections(&lines, 200000000000000.0, 400000000000000.0);
  (p1, 0)
}
