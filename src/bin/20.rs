use std::collections::VecDeque;
use hashbrown::HashMap;

enum Node<'a> {
  FlipFlop(bool),
  Conjunction(HashMap<&'a str, bool>),
  Broadcaster,
}

#[aoc::main(20)]
fn main(input: &str) -> (usize, usize) {
  let mut g = HashMap::new();
  let mut state = HashMap::new();
  for l in input.split('\n') {
    let (src, connections) = l.split_once(" -> ").unwrap();
    let (node, state_type) = match src.as_bytes()[0] {
      b'%' => (&src[1..], Node::FlipFlop(false)),
      b'&' => (&src[1..], Node::Conjunction(HashMap::new())),
      b'b' => (src,       Node::Broadcaster),
      _ => unreachable!(),
    };
    g.insert(node, connections.split(", ").collect::<Vec<_>>());
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
  let mut cycles = {
    let Node::Conjunction(m) = &state[rx_conjunction] else { panic!() };
    m.keys().map(|&node| (node, None)).collect::<HashMap<_,_>>()
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
        if v.is_none() {
          *v = Some(t);
          if cycles.values().all(|o| o.is_some()) {
            break 'outer;
          }
        }
      }
      let pulse = match state.get_mut(node) {
        Some(Node::FlipFlop(_)) if high => continue,
        Some(Node::FlipFlop(on)) => {
          *on = !*on;
          *on
        },
        Some(Node::Conjunction(m)) => {
          m.insert(prev, high);
          m.values().any(|&b| !b)
        }
        Some(Node::Broadcaster) => false,
        None => continue,
      };
      q.extend(g[node].iter().map(|&n| (n, node, pulse)));
    }
  }
  (p1[0] * p1[1], cycles.values().map(|o| o.unwrap()).product())
}
