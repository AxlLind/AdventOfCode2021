use hashbrown::HashMap;
use itertools::Itertools;

enum Op<'a> { Num(i64), Op(&'a str, char, &'a str) }

fn val(monkies: &HashMap<&str, Op>, name: &str) -> i64 {
  match &monkies[name] {
    Op::Num(i) => *i,
    Op::Op(m1, '+', m2) => val(monkies, m1) + val(monkies, m2),
    Op::Op(m1, '-', m2) => val(monkies, m1) - val(monkies, m2),
    Op::Op(m1, '*', m2) => val(monkies, m1) * val(monkies, m2),
    Op::Op(m1, '/', m2) => val(monkies, m1) / val(monkies, m2),
    _ => unreachable!(),
  }
}

fn get_eq(monkies: &HashMap<&str, Op>, name: &str) -> String {
  match &monkies[name] {
    _ if name == "humn" => "x".to_string(),
    Op::Num(i) => i.to_string(),
    Op::Op(m1, op, m2) => format!("({} {} {})", get_eq(monkies, m1), op, get_eq(monkies, m2)),
  }
}

#[aoc::main(21)]
fn main(input: &str) -> (i64, &str) {
  let monkies = input.lines().map(|l| {
    let (name, rest) = l.split_once(": ").unwrap();
    let op = rest.split(' ').collect_tuple()
      .map(|(m1,op,m2)| Op::Op(m1, op.as_bytes()[0] as char, m2))
      .unwrap_or_else(|| Op::Num(rest.parse().unwrap()));
    (name, op)
  }).collect::<HashMap<_,_>>();
  let Op::Op(a,_,b) = monkies["root"] else { panic!() };
  println!("{} = {}", val(&monkies, b), get_eq(&monkies, a));
  (val(&monkies, "root"), "glhf -> https://www.mathpapa.com/equation-solver/")
}
