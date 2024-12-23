use itertools::Itertools;
use serde_json::Value;
use std::cmp::{max, Ordering};

fn comp(a: &Value, b: &Value) -> Ordering {
  match (a,b) {
    (Value::Number(x), Value::Number(y)) => x.as_u64().unwrap().cmp(&y.as_u64().unwrap()),
    (Value::Array(a), Value::Array(b)) => {
      for i in 0..max(a.len(), b.len()) {
        match (a.get(i), b.get(i)) {
          (None, _) => return Ordering::Less,
          (_, None) => return Ordering::Greater,
          (Some(x), Some(y)) => match comp(x,y) {
            Ordering::Equal => {},
            c => return c,
          }
        }
      }
      Ordering::Equal
    },
    (Value::Array(_), Value::Number(_)) => comp(a, &Value::Array(vec![b.clone()])),
    (Value::Number(_), Value::Array(_)) => comp(&Value::Array(vec![a.clone()]), b),
    _ => unreachable!(),
  }
}

#[aoc::main(13)]
fn main(input: &str) -> (usize, usize) {
  let mut signals = input.lines()
    .filter(|l| !l.is_empty())
    .map(|l| serde_json::from_str::<Value>(l).unwrap())
    .collect::<Vec<_>>();
  let p1 = signals.iter()
    .tuples()
    .positions(|(a,b)| comp(a,b) != Ordering::Greater)
    .map(|i| i + 1)
    .sum();
  let beacons = [
    serde_json::from_str::<Value>("[[2]]").unwrap(),
    serde_json::from_str::<Value>("[[6]]").unwrap(),
  ];
  signals.extend(beacons.iter().cloned());
  signals.sort_by(comp);
  let p2 = signals.iter().positions(|b| beacons.contains(b)).map(|i| i + 1).product();
  (p1,p2)
}
