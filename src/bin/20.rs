use std::collections::VecDeque;
use hashbrown::{HashMap, HashSet};

fn gcd(a: usize, b: usize) -> usize {
  match ((a, b), (a & 1, b & 1)) {
    _ if a == b => a,
    ((_, 0), _) => a,
    ((0, _), _) => b,
    (_, (0, 1) | (1, 0)) => gcd(a >> 1, b),
    (_, (0, 0)) => gcd(a >> 1, b >> 1) << 1,
    (_, (1, 1)) => {
      let (a, b) = (a.min(b), a.max(b));
      gcd((b - a) >> 1, a)
    }
    _ => unreachable!(),
  }
}

fn lcm(vals: impl Iterator<Item=usize>) -> usize {
  vals.fold(1, |ans, x| (x*ans) / gcd(x,ans))
}

#[aoc::main(20)]
fn main(input: &str) -> (usize, usize) {
  let g = input.split('\n').map(|l| {
    let (src, rest) = l.split_once(" -> ").unwrap();
    let dests = rest.split(", ").collect::<Vec<_>>();
    if src == "broadcaster" {
      (src, ('b', dests))
    } else {
      (&src[1..], (src.as_bytes()[0] as char, dests))
    }
  }).collect::<HashMap<_,_>>();
  let mut state = HashSet::new();
  let mut conjunctions = HashMap::<&str,  HashMap<&str, bool>>::new();
  for (&node, (_, connections)) in &g {
    for &n in connections {
      let Some((tpe, _)) = g.get(n) else { continue };
      if *tpe == '&' {
        conjunctions.entry(n).or_default().insert(node, false);
      }
    }
  }

  let mut p1 = 0;
  let (mut l, mut h) = (0,0);
  let mut cycles = [None; 4];
  for t in 0.. {
    if t == 1000 {
      p1 = l * h;
    }
    let mut q = VecDeque::from_iter([("broadcaster", "button", false)]);
    while let Some((node, prev, high)) = q.pop_front() {
      if high && node == "mf" {
        let i = match prev {
          "bh" => 0,
          "jf" => 1,
          "sh" => 2,
          "mz" => 3,
          _ => unreachable!(),
        };
        cycles[i] = cycles[i].or(Some(t + 1));
      }
      if high {
        h += 1;
      } else {
        l += 1;
      }
      let Some((tpe, connections)) = g.get(node) else { continue };
      let pulse = match tpe {
        'b' => false,
        '%' => {
          if high {
            continue;
          }
          let on = state.contains(node);
          if on {
            state.remove(node);
          } else {
            state.insert(node);
          }
          !on
        }
        '&' => {
          conjunctions.get_mut(node).unwrap().insert(prev, high);
          !conjunctions[node].values().all(|&b| b)
        }
        _ => unreachable!(),
      };
      q.extend(connections.iter().map(|&n| (n, node, pulse)));
    }
    if cycles.iter().all(|o| o.is_some()) {
      break;
    }
  }
  (p1, lcm(cycles.iter().map(|o| o.unwrap())))
}
