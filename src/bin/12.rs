use std::time::Instant;
use std::cmp::Ordering;

#[derive(Default, Debug)]
struct Vec3 { x:i64, y:i64, z:i64 }

#[derive(Debug)]
struct Moon { pos: Vec3, v: Vec3 }

impl Moon {
  fn from_pos(x: i64, y: i64, z: i64) -> Self {
    Self {
      pos: Vec3{ x,y,z },
      v: Vec3::default(),
    }
  }

  fn get_energy(&self) -> i64 {
    let pot = self.pos.x.abs() + self.pos.y.abs() + self.pos.z.abs();
    let kin = self.v.x.abs() + self.v.y.abs() + self.v.z.abs();
    pot * kin
  }
}

fn cmp_axis(a: i64, b: i64) -> i64 {
  match a.cmp(&b) {
    Ordering::Less    =>  1,
    Ordering::Equal   =>  0,
    Ordering::Greater => -1,
  }
}

fn main() {
  let now = Instant::now();
  let mut moons = [
    // Moon::from_pos(-10, -10, -13),
    // Moon::from_pos(5, 5, -9),
    // Moon::from_pos(3, 8, -16),
    // Moon::from_pos(1, 3, -3),
    Moon::from_pos(-1,0,2),
    Moon::from_pos(2,-10,-7),
    Moon::from_pos(4,-8,8),
    Moon::from_pos(3,5,-1),
  ];
  for _ in 0..100 {
    // calculate velocities
    let mut velocities = vec![];
    for m1 in &moons {
      let mut v = Vec3::default();
      for m2 in &moons {
        v.x += cmp_axis(m1.pos.x, m2.pos.x);
        v.y += cmp_axis(m1.pos.y, m2.pos.y);
        v.z += cmp_axis(m1.pos.z, m2.pos.z);
      }
      velocities.push(v);
    }

    // update velocities and positions
    for (i,v) in velocities.iter().enumerate() {
      moons[i].v.x += v.x;
      moons[i].v.y += v.y;
      moons[i].v.z += v.z;
      moons[i].pos.x += moons[i].v.x;
      moons[i].pos.y += moons[i].v.y;
      moons[i].pos.z += moons[i].v.z;
    }
  }
  let answer = moons.iter().map(|m| m.get_energy()).sum::<i64>();
  println!("{:?}", moons);
  println!("{}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
