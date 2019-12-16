use std::time::Instant;
use std::cmp::Ordering;
use std::iter;
use num_integer::lcm;

fn cmp_axis(a: i64, b: i64) -> i64 {
  match a.cmp(&b) {
    Ordering::Less    =>  1,
    Ordering::Equal   =>  0,
    Ordering::Greater => -1,
  }
}

fn simulate_one_axis(positions: [i64;4]) -> usize {
  let mut moons = [
    (positions[0],0),
    (positions[1],0),
    (positions[2],0),
    (positions[3],0),
  ];
  let init = moons;
  let steps = iter::repeat(()).take_while(|_| {
    for i in 0..4 {
      for j in (i+1)..4 {
        let d = cmp_axis(moons[i].0, moons[j].0);
        moons[i].1 += d;
        moons[j].1 -= d;
      }
      moons[i].0 += moons[i].1;
    }
    moons != init
  }).count();
  steps + 1
}

fn main() {
  let now = Instant::now();
  let x = simulate_one_axis([-10,  5,   3,  1]);
  let y = simulate_one_axis([-10,  5,   8,  3]);
  let z = simulate_one_axis([-13, -9, -16, -3]);
  let answer = lcm(x,lcm(y,z));
  println!("{}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
