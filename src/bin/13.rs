use std::time::Instant;

static START: i64 = 1003240;
static INPUT: &str = "19,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,787,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,29,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17";

fn part_one(busses: &[(i64,i64)]) -> i64 {
  for i in START.. {
    if let Some((_,b)) = busses.iter().find(|(_,b)| i % b == 0) {
      return b * (i - START)
    }
  }
  unreachable!()
}

fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
  if a == 0 {
    (b, 0, 1)
  } else {
    let (g, x, y) = egcd(b % a, a);
    (g, y - (b / a) * x, x)
  }
}

fn mod_inv(x: i64, n: i64) -> Option<i64> {
  let (g, x, _) = egcd(x, n);
  if g == 1 {
    Some((x % n + n) % n)
  } else {
    None
  }
}

// from: https://rosettacode.org/wiki/Chinese_remainder_theorem#Rust
fn chinese_remainder(residues: &[i64], modulii: &[i64]) -> Option<i64> {
  let prod = modulii.iter().product::<i64>();
  let mut sum = 0;
  for (&residue, &modulus) in residues.iter().zip(modulii) {
    let p = prod / modulus;
    sum += residue * mod_inv(p, modulus)? * p
  }
  Some(sum % prod)
}

fn part_two(busses: &[(i64, i64)]) -> i64 {
  let mods = busses.iter().map(|&(_,b)| b).collect::<Vec<_>>();
  let res  = busses.iter().map(|&(i,b)| b-i).collect::<Vec<_>>();
  chinese_remainder(&res, &mods).unwrap()
}

fn main() {
  let now = Instant::now();
  let busses = INPUT.split(",")
    .enumerate()
    .filter(|&(_,s)| s != "x")
    .map(|(i,s)| (i as i64, s.parse().unwrap()))
    .collect::<Vec<_>>();
  println!("Part one: {}", part_one(&busses));
  println!("Part two: {}", part_two(&busses));
  println!("Time: {}ms", now.elapsed().as_millis());
}
