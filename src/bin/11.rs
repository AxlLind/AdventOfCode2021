use itertools::Itertools;

#[derive(Clone, Copy, Debug)]
enum Op { Add(u64), Mul(u64), Special }

fn simulate(mut monkies: Vec<(Vec<u64>, Op, u64, usize, usize)>, rounds: usize, f: impl Fn(u64) -> u64) -> usize {
  let mut inspections = vec![0; monkies.len()];
  for _ in 0..rounds {
    for i in 0..monkies.len() {
      let (items, op, div, m1, m2) = monkies[i].clone();
      for item in items {
        let worry = match op {
          Op::Add(v) => f(item + v),
          Op::Mul(v) => f(item * v),
          Op::Special => f(item * item),
        };
        monkies[if worry % div == 0 {m1} else {m2}].0.push(worry);
      }
      inspections[i] += monkies[i].0.len();
      monkies[i].0.clear();
    }
  }
  inspections.sort_by_key(|&x| -(x as isize));
  inspections[0] * inspections[1]
}

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
  let monkies = input.split("\n\n").map(|m| {
    let (l1, l2, l3, l4, l5) = m.lines().skip(1).map(str::trim).collect_tuple().unwrap();
    (
      l1["Starting items: ".len()..].split(", ").map(|x| x.parse().unwrap()).collect(),
      l2["Operation: new = old * ".len()..].parse().map(|v| if l2.contains('+') {Op::Add(v)} else {Op::Mul(v)}).unwrap_or(Op::Special),
      l3["Test: divisible by ".len()..].parse().unwrap(),
      l4["If true: throw to monkey ".len()..].parse().unwrap(),
      l5["If false: throw to monkey ".len()..].parse().unwrap(),
    )
  }).collect::<Vec<_>>();
  let modulus = monkies.iter().map(|m| m.2).product::<u64>();
  let p1 = simulate(monkies.clone(), 20, |x| x / 3);
  let p2 = simulate(monkies, 10000, |x| x % modulus);
  (p1, p2)
}
