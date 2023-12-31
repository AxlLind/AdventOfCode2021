use z3::ast::{Ast, Int, Real};
use itertools::Itertools;

fn part1(hailstones: &[(f64,f64,f64,f64,f64,f64)]) -> usize {
  hailstones.iter()
    .tuple_combinations()
    .filter(|(&(x1,y1,_,dx1,dy1,_), &(x2,y2,_,dx2,dy2,_))| {
      let m1 = dy1 / dx1;
      let m2 = dy2 / dx2;
      if (m2 - m1).abs() <= f64::EPSILON {
        return false;
      }
      let x = (m1 * x1 - m2*x2 + y2 - y1) / (m1 - m2);
      let y = (m1*m2*(x2-x1) + m2*y1 - m1*y2) / (m2 - m1);
      if dx1.signum() != (x-x1).signum() || dx2.signum() != (x-x2).signum() {
        return false;
      }
      [x,y].iter().all(|v| (200000000000000.0..=400000000000000.0).contains(v))
    })
    .count()
}

fn part2(hailstones: &[(f64,f64,f64,f64,f64,f64)]) -> usize {
  let ctx = z3::Context::new(&z3::Config::new());
  let s = z3::Solver::new(&ctx);
  let [fx,fy,fz,fdx,fdy,fdz] = ["fx","fy","fz","fdx","fdy","fdz"].map(|v| Real::new_const(&ctx, v));
  let zero = Int::from_i64(&ctx, 0).to_real();
  for (i, &(x,y,z,dx,dy,dz)) in hailstones[..3].iter().enumerate() {
    let [x,y,z,dx,dy,dz] = [x,y,z,dx,dy,dz].map(|v| Int::from_i64(&ctx, v as _).to_real());
    let t = Real::new_const(&ctx, format!("t{i}"));
    s.assert(&t.ge(&zero));
    s.assert(&((&x + &dx * &t)._eq(&(&fx + &fdx * &t))));
    s.assert(&((&y + &dy * &t)._eq(&(&fy + &fdy * &t))));
    s.assert(&((&z + &dz * &t)._eq(&(&fz + &fdz * &t))));
  }
  assert_eq!(s.check(), z3::SatResult::Sat);
  let res = s.get_model().unwrap().eval(&(&fx + &fy + &fz), true).unwrap();
  res.to_string().strip_suffix(".0").unwrap().parse().unwrap()
}

#[aoc::main(24)]
fn main(input: &str) -> (usize, usize) {
  let hailstones = input.split('\n').map(|l|
    l.split(['@', ','])
      .map(|w| w.trim().parse::<f64>().unwrap())
      .collect_tuple()
      .unwrap()
  ).collect::<Vec<_>>();
  (part1(&hailstones), part2(&hailstones))
}
