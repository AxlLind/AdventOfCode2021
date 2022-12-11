#[derive(Clone, Copy, Debug)]
enum Op { Add(u32), Mul(u32), Special }

fn simulate(mut monkies: Vec<(Vec<u32>, Op, u32, usize, usize)>, rounds: usize, f: impl Fn(u32) -> u32) -> usize {
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
    let mut items = m.split_whitespace()
      .filter_map(|x|
        x.parse::<u32>().or(x[0..x.len()-1].parse::<u32>()).ok()
      )
      .skip(1)
      .collect::<Vec<_>>();
    let rest_len = if m.contains("old * old") {3} else {4};
    let mut rest = items.split_off(items.len() - rest_len);
    let op = if m.contains("old * old") {
      Op::Special
    } else if m.contains(" + ") {
      Op::Add(rest.remove(0))
    } else {
      Op::Mul(rest.remove(0))
    };
    (items, op, rest[0], rest[1] as usize, rest[2] as usize)
  }).collect::<Vec<_>>();
  let modulus = monkies.iter().map(|m| m.2).product::<u32>();
  let p1 = simulate(monkies.clone(), 20, |x| x / 3);
  let p2 = simulate(monkies.clone(), 1000, |x| x % modulus);
  (p1,p2)
}
