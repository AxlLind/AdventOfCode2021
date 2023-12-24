use z3::ast::{Ast, Int};
use itertools::Itertools;

fn intersection((x1,y1,dx1,dy1): (f64,f64,f64,f64), (x2,y2,dx2,dy2): (f64,f64,f64,f64)) -> Option<(f64, f64)> {
  let m1 = dy1 / dx1;
  let m2 = dy2 / dx2;
  if (m2 - m1).abs() < f64::EPSILON {
    return None;
  }
  let x = (m1 * x1 - m2*x2 + y2 - y1) / (m1 - m2);
  let y = (m1*m2*(x2-x1) + m2*y1 - m1*y2) / (m2 - m1);
  Some((x,y))
}

fn find_intersections(lines: &[((f64,f64,f64),(f64,f64,f64))]) -> usize {
  let range = 200000000000000.0..=400000000000000.0;
  lines.iter()
    .tuple_combinations()
    .filter(|(&((x1,y1,_),(dx1,dy1,_)), &((x2,y2,_),(dx2,dy2,_)))| {
      let Some((x,y)) = intersection((x1,y1,dx1,dy1), (x2,y2,dx2,dy2)) else { return false };
      if dx1.signum() != (x-x1).signum() || dx2.signum() != (x-x2).signum() {
        return false;
      }
      range.contains(&x) && range.contains(&y)
    })
    .count()
}

fn part2(lines: &[((f64,f64,f64),(f64,f64,f64))]) -> i64 {
  let ctx = z3::Context::new(&z3::Config::new());
  let s = z3::Solver::new(&ctx);
  let [fx,fy,fz] = ["fx", "fy", "fz"].map(|v| Int::new_const(&ctx, v));
  let [fdx,fdy,fdz] = ["fdx", "fdy", "fdz"].map(|v| Int::new_const(&ctx, v));

  let zero = Int::from_i64(&ctx, 0);
  for (i, &((x,y,z), (dx,dy,dz))) in lines.iter().enumerate() {
    let [x,y,z] = [x,y,z].map(|v| Int::from_i64(&ctx, v as _));
    let [dx,dy,dz] = [dx,dy,dz].map(|v| Int::from_i64(&ctx, v as _));
    let t = Int::new_const(&ctx, format!("t{i}"));
    s.assert(&t.ge(&zero));
    s.assert(&((&x + &dx * &t)._eq(&(&fx + &fdx * &t))));
    s.assert(&((&y + &dy * &t)._eq(&(&fy + &fdy * &t))));
    s.assert(&((&z + &dz * &t)._eq(&(&fz + &fdz * &t))));
  }
  assert_eq!(s.check(), z3::SatResult::Sat);
  let model = s.get_model().unwrap();
  let res = model.eval(&(&fx + &fy + &fz), true).unwrap();
  res.as_i64().unwrap()
}

#[aoc::main(24)]
fn main(input: &str) -> (usize, i64) {
  let lines = input.split('\n').map(|l| {
    let (a,b) = l.split_once(" @ ").unwrap();
    let (x,y,z) = a.split(", ").map(|w| w.parse::<f64>().unwrap()).collect_tuple().unwrap();
    let (dx,dy,dz) = b.split(", ").map(|w| w.trim().parse::<f64>().unwrap()).collect_tuple().unwrap();
    ((x,y,z), (dx,dy,dz))
  }).collect::<Vec<_>>();
  (find_intersections(&lines), part2(&lines))
}
