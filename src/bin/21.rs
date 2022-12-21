use hashbrown::HashMap;
use itertools::Itertools;

enum Op {
  Num(i64),
  Add(String, String),
  Sub(String, String),
  Mul(String, String),
  Div(String, String),
}

fn search(monkies: &HashMap<String, Op>, cache: &mut HashMap<String, i64>, name: &str) -> i64 {
  if let Some(&v) = cache.get(name) {
    return v;
  }
  let v = match &monkies[name] {
    Op::Num(i) => *i,
    Op::Add(m1, m2) => {
      let a = search(monkies, cache, &m1);
      let b = search(monkies, cache, &m2);
      a + b
    }
    Op::Sub(m1, m2) => {
      let a = search(monkies, cache, &m1);
      let b = search(monkies, cache, &m2);
      a - b
    }
    Op::Mul(m1, m2) => {
      let a = search(monkies, cache, &m1);
      let b = search(monkies, cache, &m2);
      a * b
    }
    Op::Div(m1, m2) => {
      let a = search(monkies, cache, &m1);
      let b = search(monkies, cache, &m2);
      a / b
    }
  };
  cache.insert(name.to_string(), v);
  v
}

fn print_eq(monkies: &HashMap<String, Op>, name: &str) -> String {
  match &monkies[name] {
    Op::Num(i) => i.to_string(),
    Op::Add(m1, m2) => {
      let a = print_eq(monkies, &m1);
      let b = print_eq(monkies, &m2);
      format!("({} + {})", a, b)
    }
    Op::Sub(m1, m2) => {
      let a = print_eq(monkies, &m1);
      let b = print_eq(monkies, &m2);
      format!("({} - {})", a, b)
    }
    Op::Mul(m1, m2) => {
      let a = print_eq(monkies, &m1);
      let b = print_eq(monkies, &m2);
      format!("({} * {})", a, b)
    }
    Op::Div(m1, m2) => {
      let a = print_eq(monkies, &m1);
      let b = print_eq(monkies, &m2);
      format!("({} / {})", a, b)
    }
  }
}

#[aoc::main(21)]
fn main(input: &str) -> (i64, &str) {
  let mut monkies = input.lines().map(|l| {
    let (name, rest) = l.split_once(": ").unwrap();
    let op = if !rest.contains(' ') {
      Op::Num(rest.parse().unwrap())
    } else {
      let (m1, op, m2) = rest.split(' ').collect_tuple().unwrap();
      match op {
        "+" => Op::Add(m1.to_string(), m2.to_string()),
        "-" => Op::Sub(m1.to_string(), m2.to_string()),
        "*" => Op::Mul(m1.to_string(), m2.to_string()),
        "/" => Op::Div(m1.to_string(), m2.to_string()),
        _ => unreachable!(),
      }
    };
    (name.to_string(), op)
  }).collect::<HashMap<_,_>>();
  let mut values = HashMap::new();
  monkies.insert("humn".to_string(), Op::Num(-1337));
  println!("{} = {}", search(&monkies, &mut values, "tfjf"), print_eq(&monkies, "vtsj").replace("-1337", "x"));
  let p1 = search(&monkies, &mut values, "root");
  (p1,"glhf")
}
