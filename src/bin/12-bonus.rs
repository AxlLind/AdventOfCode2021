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

fn step(moons: &mut [(i64,i64);4]) {
  for i in 0..4 {
    for j in (i+1)..4 {
      let d = cmp_axis(moons[i].0, moons[j].0);
      moons[i].1 += d;
      moons[j].1 -= d;
    }
    moons[i].0 += moons[i].1;
  }
}

fn simulate_one_axis(init: [i64;4]) -> usize {
  let init = [
    (init[0],0),
    (init[1],0),
    (init[2],0),
    (init[3],0)
  ];
  let mut moons = init.clone();
  1 + iter::repeat(()).take_while(|_| {
    step(&mut moons);
    moons != init
  }).count()
}

fn main() {
  let now = Instant::now();
  let x = simulate_one_axis([-10, 5, 3, 1]);
  let y = simulate_one_axis([-10, 5, 8, 3]);
  let z = simulate_one_axis([-13, -9, -16, -3]);
  let answer = lcm(x,lcm(y,z));
  println!("{}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
