use std::time::Instant;
use std::cmp::Ordering;
use num_integer::lcm;

#[derive(Default)]
struct Vec3 { x:i64, y:i64, z:i64 }

impl Vec3 {
  fn to_val(&self) -> i64 {
    let Vec3{x,y,z} = self;
    x.abs() + y.abs() + z.abs()
  }
}

struct Moon { pos: Vec3, v: Vec3 }

impl Moon {
  fn from_pos(x: i64, y: i64, z: i64) -> Self {
    Self { pos: Vec3{x,y,z}, v: Vec3::default() }
  }

  fn energy(&self) -> i64 { self.pos.to_val() * self.v.to_val() }
}

fn cmp_axis(a: i64, b: i64) -> i64 {
  match a.cmp(&b) {
    Ordering::Less    =>  1,
    Ordering::Equal   =>  0,
    Ordering::Greater => -1,
  }
}

fn part_one() -> i64 {
  let mut moons = [
    Moon::from_pos(-10, -10, -13),
    Moon::from_pos(5, 5, -9),
    Moon::from_pos(3, 8, -16),
    Moon::from_pos(1, 3, -3),
  ];
  for _ in 0..1000 {
    for i in 0..4 {
      for j in (i+1)..4 {
        let dx = cmp_axis(moons[i].pos.x, moons[j].pos.x);
        moons[i].v.x += dx;
        moons[j].v.x -= dx;
        let dy = cmp_axis(moons[i].pos.y, moons[j].pos.y);
        moons[i].v.y += dy;
        moons[j].v.y -= dy;
        let dz = cmp_axis(moons[i].pos.z, moons[j].pos.z);
        moons[i].v.z += dz;
        moons[j].v.z -= dz;
      }
      moons[i].pos.x += moons[i].v.x;
      moons[i].pos.y += moons[i].v.y;
      moons[i].pos.z += moons[i].v.z;
    }
  }
  moons.iter().map(|m| m.energy()).sum::<i64>()
}

fn simulate_one_axis(positions: [i64;4]) -> usize {
  let mut moons = [
    (positions[0],0),
    (positions[1],0),
    (positions[2],0),
    (positions[3],0),
  ];
  let init = moons;
  for c in 1.. {
    for i in 0..4 {
      for j in (i+1)..4 {
        let d = cmp_axis(moons[i].0, moons[j].0);
        moons[i].1 += d;
        moons[j].1 -= d;
      }
      moons[i].0 += moons[i].1;
    }
    if moons == init { return c; }
  }
  unreachable!()
}

fn part_two() -> usize {
  let x = simulate_one_axis([-10,  5,   3,  1]);
  let y = simulate_one_axis([-10,  5,   8,  3]);
  let z = simulate_one_axis([-13, -9, -16, -3]);
  lcm(x,lcm(y,z))
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
