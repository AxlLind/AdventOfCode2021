use itertools::Itertools;

#[derive(Clone, Copy, Debug)]
enum Op { Add(u64), Mul(u64), Special }

fn simulate(mut monkies: Vec<(Vec<u64>, Op, u64, usize, usize)>, rounds: usize, f: impl Fn(u64) -> u64) -> usize {
  let mut inspections = vec![0; monkies.len()];
  for _ in 0..rounds {
    for i in 0..monkies.len() {
      let (items, op, div, true_m, false_m) = monkies[i].clone();
      for item in items {
        let worry = match op {
          Op::Add(v) => f(item + v),
          Op::Mul(v) => f(item * v),
          Op::Special => f(item * item),
        };
        monkies[if worry % div == 0 {true_m} else {false_m}].0.push(worry);
      }
      inspections[i] += monkies[i].0.len();
      monkies[i].0.clear();
    }
  }
  inspections.sort();
  inspections.reverse();
  inspections[0] * inspections[1]
}

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
  let monkies = input.split("\n\n").map(|m| {
    let (l1, l2, l3, l4, l5) = m.lines().skip(1).map(|l| l.trim()).collect_tuple().unwrap();
    let items = l1["Starting items: ".len()..].split(", ").map(|x| x.parse::<u64>().unwrap()).collect::<Vec<_>>();
    let op = match l2["Operation: new = old * ".len()..].parse::<u64>() {
      Ok(v) => if l2.contains('+') {Op::Add(v)} else {Op::Mul(v)},
      _ => Op::Special
    };
    let div = l3["Test: divisible by ".len()..].parse::<u64>().unwrap();
    let m1 = l4["If true: throw to monkey ".len()..].parse::<usize>().unwrap();
    let m2 = l5["If false: throw to monkey ".len()..].parse::<usize>().unwrap();
    (items, op, div, m1, m2)
  }).collect::<Vec<_>>();
  let modulus = monkies.iter().map(|m| m.2).product::<u64>();
  let p1 = simulate(monkies.clone(), 20, |x| x / 3);
  let p2 = simulate(monkies, 10000, |x| x % modulus);
  (p1,p2)
}
