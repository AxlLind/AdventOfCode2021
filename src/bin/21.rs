use hashbrown::HashMap;
use itertools::Itertools;

enum Op<'a> {
  Num(i64),
  Add(&'a str, &'a str),
  Sub(&'a str, &'a str),
  Mul(&'a str, &'a str),
  Div(&'a str, &'a str),
}

fn val<'a>(monkies: &'a HashMap<&str, Op>, cache: &mut HashMap<&'a str, i64>, name: &'a str) -> i64 {
  if let Some(&v) = cache.get(name) {
    return v;
  }
  let v = match &monkies[name] {
    Op::Num(i) => *i,
    Op::Add(m1, m2) => val(monkies, cache, &m1) + val(monkies, cache, &m2),
    Op::Sub(m1, m2) => val(monkies, cache, &m1) - val(monkies, cache, &m2),
    Op::Mul(m1, m2) => val(monkies, cache, &m1) * val(monkies, cache, &m2),
    Op::Div(m1, m2) => val(monkies, cache, &m1) / val(monkies, cache, &m2),
  };
  cache.insert(name, v);
  v
}

fn get_eq(monkies: &HashMap<&str, Op>, name: &str) -> String {
  if name == "humn" {
    return "x".to_string();
  }
  match &monkies[name] {
    Op::Num(i) => i.to_string(),
    Op::Add(m1, m2) => format!("({} + {})", get_eq(monkies, &m1), get_eq(monkies, &m2)),
    Op::Sub(m1, m2) => format!("({} - {})", get_eq(monkies, &m1), get_eq(monkies, &m2)),
    Op::Mul(m1, m2) => format!("({} * {})", get_eq(monkies, &m1), get_eq(monkies, &m2)),
    Op::Div(m1, m2) => format!("({} / {})", get_eq(monkies, &m1), get_eq(monkies, &m2)),
  }
}

#[aoc::main(21)]
fn main(input: &str) -> (i64, &str) {
  let monkies = input.lines().map(|l| {
    let (name, rest) = l.split_once(": ").unwrap();
    let op = match rest.split(' ').collect_tuple() {
      Some((m1, "+", m2)) => Op::Add(m1, m2),
      Some((m1, "-", m2)) => Op::Sub(m1, m2),
      Some((m1, "*", m2)) => Op::Mul(m1, m2),
      Some((m1, "/", m2)) => Op::Div(m1, m2),
      _ => Op::Num(rest.parse().unwrap()),
    };
    (name, op)
  }).collect();
  let mut values = HashMap::new();
  println!("{} = {}", val(&monkies, &mut values, "tfjf"), get_eq(&monkies, "vtsj"));
  (val(&monkies, &mut values, "root"), "glhf -> https://www.mathpapa.com/equation-solver/")
}
