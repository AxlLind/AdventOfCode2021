use std::time::Instant;
use std::cmp::Ordering;

#[derive(Default)]
struct Vec3 { x:i64, y:i64, z:i64 }

struct Moon { pos: Vec3, v: Vec3 }

impl Moon {
  fn from_pos(x: i64, y: i64, z: i64) -> Self {
    Self { pos: Vec3{ x,y,z }, v: Vec3::default() }
  }

  fn energy(&self) -> i64 {
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
  let answer = moons.iter().map(|m| m.energy()).sum::<i64>();
  println!("{}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
