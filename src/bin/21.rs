use hashbrown::HashMap;
use itertools::Itertools;

enum Op {
  Num(i64),
  Add(String, String),
  Sub(String, String),
  Mul(String, String),
  Div(String, String),
}

fn val(monkies: &HashMap<String, Op>, cache: &mut HashMap<String, i64>, name: &str) -> i64 {
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
  cache.insert(name.to_string(), v);
  v
}

fn get_eq(monkies: &HashMap<String, Op>, name: &str) -> String {
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
  let mut monkies = input.lines().map(|l| {
    let (name, rest) = l.split_once(": ").unwrap();
    let op = match rest.split(' ').collect_tuple() {
      Some((m1, "+", m2)) => Op::Add(m1.to_string(), m2.to_string()),
      Some((m1, "-", m2)) => Op::Sub(m1.to_string(), m2.to_string()),
      Some((m1, "*", m2)) => Op::Mul(m1.to_string(), m2.to_string()),
      Some((m1, "/", m2)) => Op::Div(m1.to_string(), m2.to_string()),
      _ => Op::Num(rest.parse().unwrap()),
    };
    (name.to_string(), op)
  }).collect::<HashMap<_,_>>();
  let mut values = HashMap::new();
  let p1 = val(&monkies, &mut values, "root");
  monkies.insert("humn".to_string(), Op::Num(-1337));
  println!("{} = {}", val(&monkies, &mut values, "tfjf"), get_eq(&monkies, "vtsj").replace("-1337", "x"));
  (p1,"glhf -> https://www.mathpapa.com/equation-solver/")
}
