use std::collections::VecDeque;
use hashbrown::HashMap;

enum Node<'a> {
  FlipFlop(bool),
  Conjunction(HashMap<&'a str, bool>),
  Broadcaster,
}

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
  let mut g = HashMap::new();
  let mut state = HashMap::new();
  for l in input.split('\n') {
    let (src, rest) = l.split_once(" -> ").unwrap();
    let connections = rest.split(", ").collect::<Vec<_>>();
    let (node, state_type) = match src.as_bytes()[0] as char {
      '%' => (&src[1..], Node::FlipFlop(false)),
      '&' => (&src[1..], Node::Conjunction(HashMap::new())),
      'b' => (src,       Node::Broadcaster),
      _ => unreachable!(),
    };
    g.insert(node, connections);
    state.insert(node, state_type);
  }

  let mut rx_conjunction = "";
  for (&node, connections) in &g {
    for &n in connections {
      match state.get_mut(n) {
        Some(Node::Conjunction(m)) => { m.insert(node, false); },
        Some(_) => {},
        None => rx_conjunction = node,
      }
    }
  }
  let mut cycles = match &state[rx_conjunction] {
    Node::Conjunction(m) => m.iter()
      .map(|(&node,_)| (node, None))
      .collect::<HashMap<_,_>>(),
    _ => unreachable!(),
  };

  let mut p1 = [0,0];
  let mut q = VecDeque::new();
  'outer: for t in 1.. {
    q.push_back(("broadcaster", "button", false));
    while let Some((node, prev, high)) = q.pop_front() {
      if t <= 1000 {
        p1[high as usize] += 1;
      }
      if high && node == rx_conjunction {
        let v = cycles.get_mut(prev).unwrap();
        *v = v.or(Some(t));
        if cycles.values().all(|o| o.is_some()) {
          break 'outer;
        }
      }
      let Some(state) = state.get_mut(node) else { continue };
      let pulse = match (state, high) {
        (Node::FlipFlop(_), true) => continue,
        (Node::FlipFlop(on), false) => {
          *on = !*on;
          *on
        },
        (Node::Conjunction(m), _) => {
          m.insert(prev, high);
          m.values().any(|&b| !b)
        }
        (Node::Broadcaster, _) => false,
      };
      q.extend(g[node].iter().map(|&n| (n, node, pulse)));
    }
  }
  (p1[0] * p1[1], lcm(cycles.values().map(|o| o.unwrap())))
}
