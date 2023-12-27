use std::collections::VecDeque;
use hashbrown::{HashMap, HashSet};

fn mincut_edmond_karp(g: &HashMap<&str, HashSet<&str>>, s: &str, t: &str, cut: i64) -> Option<usize> {
  let mut flow = HashMap::new();
  let mut pred = HashMap::new();
  let mut queue = VecDeque::new();
  let mut f = 0;
  while f <= cut {
    pred.clear();
    queue.clear();
    queue.push_back(s);
    let mut seen_vertices = 0;
    while let Some(cur) = queue.pop_front() {
      if pred.contains_key(t) {
        break;
      }
      seen_vertices += 1;
      for &next in &g[cur] {
        if next != s && !pred.contains_key(next) && *flow.get(&(cur, next)).unwrap_or(&0) < 1 {
          pred.insert(next, cur);
          queue.push_back(next);
        }
      }
    }
    if !pred.contains_key(t) {
      if seen_vertices == g.len() {
        return None;
      }
      return Some(seen_vertices * (g.len() - seen_vertices));
    }

    let mut df = i64::MAX;
    let mut cur = t;
    while let Some(&prev) = pred.get(cur) {
      df = df.min(1 - *flow.get(&(prev, cur)).unwrap_or(&0));
      cur = prev;
    }
    let mut cur = t;
    while let Some(&prev) = pred.get(cur) {
      *flow.entry((prev, cur)).or_default() += df;
      *flow.entry((cur, prev)).or_default() -= df;
      cur = prev;
    }
    f += df;
  }
  None
}

#[aoc::main(25)]
fn main(input: &str) -> (usize, char) {
  let mut graph = HashMap::<_, HashSet<_>>::new();
  for l in input.split('\n') {
    let (a, rest) = l.split_once(": ").unwrap();
    for b in rest.split(' ') {
      graph.entry(a).or_default().insert(b);
      graph.entry(b).or_default().insert(a);
    }
  }
  let &start = graph.keys().next().unwrap();
  (graph.keys().find_map(|k| mincut_edmond_karp(&graph, start, k, 3)).unwrap(), '🎄')
}
