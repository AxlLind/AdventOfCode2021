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

  let mut rx_conjunction = "";
  let mut state = HashSet::new();
  let mut conjunctions = HashMap::<&str,  HashMap<&str, bool>>::new();
  for (&node, (_, connections)) in &g {
    for &n in connections {
      match g.get(n) {
        Some(('&', _)) => { conjunctions.entry(n).or_default().insert(node, false); },
        Some(_) => {},
        None => rx_conjunction = node,
      }
    }
  }

  let mut p1 = 0;
  let (mut l, mut h) = (0,0);
  let mut cycles = conjunctions[rx_conjunction].iter()
    .map(|(&node,_)| (node, None))
    .collect::<HashMap<_,_>>();
  let mut q = VecDeque::new();
  for t in 0.. {
    if t == 1000 {
      p1 = l * h;
    }
    q.push_back(("broadcaster", "button", false));
    while let Some((node, prev, high)) = q.pop_front() {
      if high && node == rx_conjunction {
        let v = cycles.get_mut(prev).unwrap();
        *v = v.or(Some(t+1));
      }
      if high {
        h += 1;
      } else {
        l += 1;
      }
      let Some((tpe, connections)) = g.get(node) else { continue };
      let pulse = match (tpe, high) {
        ('%', true) => continue,
        ('%', false) => {
          let off = state.insert(node);
          if !off {
            state.remove(node);
          }
          off
        },
        ('&', _) => {
          conjunctions.get_mut(node).unwrap().insert(prev, high);
          !conjunctions[node].values().all(|&b| b)
        }
        ('b', _) => false,
        _ => unreachable!(),
      };
      q.extend(connections.iter().map(|&n| (n, node, pulse)));
    }
    if cycles.values().all(|o| o.is_some()) {
      break;
    }
  }
  (p1, lcm(cycles.values().map(|o| o.unwrap())))
}
