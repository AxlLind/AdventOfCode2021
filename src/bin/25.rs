use hashbrown::{HashMap, HashSet};

fn maximum_adjacency_search<'a>(
  g: &HashMap<&'a str, HashMap<&'a str, i64>>,
  start: &'a str,
) -> (&'a str, &'a str, i64) {
  let (mut s, mut t, mut w) = ("", start, 0);
  let mut candidates = g.keys()
    .filter(|&&k| k != start)
    .map(|&k| (k, g.get(start).and_then(|m| m.get(k)).copied().unwrap_or(0)))
    .collect::<HashMap<_,_>>();
  while !candidates.is_empty() {
    let &best_node = candidates.iter().max_by_key(|&(_, &v)| v).unwrap().0;
    s = t;
    t = best_node;
    w = candidates.remove(best_node).unwrap();
    for (&k, &v) in &g[best_node] {
      candidates.entry(k).and_modify(|w| *w += v);
    }
  }
  (s, t, w)
}

fn merge<'a>(
  g: &mut HashMap<&'a str, HashMap<&'a str, i64>>,
  s: &'a str,
  t: &'a str,
) {
  for (k, v) in g.remove(t).unwrap() {
    if k == s {
      continue;
    }
    let m = g.get_mut(k).unwrap();
    m.remove(t);
    *m.entry(s).or_default() += v;
    let m = g.get_mut(s).unwrap();
    m.remove(t);
    *m.entry(k).or_default() += v;
  }
}

fn reachable(
  g: &HashMap<&str, HashMap<&str, i64>>,
  merges: &HashMap<&str, HashSet<&str>>,
  s: &str,
  t: &str,
) -> usize {
  let mut result = HashSet::<&str>::new();
  let mut stack = vec![s];
  while let Some(n) = stack.pop() {
    result.extend(&merges[n]);
    let Some(edges) = g.get(n) else { continue };
    stack.extend(edges.keys().filter(|&&e| !result.contains(e) && e != t));
  }
  result.len()
}

fn min_cut_stoer_wagner<'a>(mut g: HashMap<&'a str, HashMap<&'a str, i64>>) -> usize {
  let &start = g.iter().next().unwrap().0;
  let mut best_cut = i64::MAX;
  let mut best_partition = 0;
  let mut merges = g.keys()
    .map(|&k| (k, HashSet::from_iter([k])))
    .collect::<HashMap<_, _>>();
  while g.len() > 1 {
    let (s, t, w) = maximum_adjacency_search(&g, start);
    if w < best_cut {
      best_partition = reachable(&g, &merges, s, t);
      best_cut = w;
    }
    merge(&mut g, s, t);
    let t_set = merges.remove(t).unwrap();
    merges.entry(s).and_modify(|e| e.extend(t_set));
  }
  best_partition
}

#[aoc::main(25)]
fn main(input: &str) -> (usize, char) {
  let mut graph = HashMap::<_, HashMap<_,_>>::new();
  for l in input.split('\n') {
    let (a, rest) = l.split_once(": ").unwrap();
    for b in rest.split(' ') {
      graph.entry(a).or_default().insert(b, 1);
      graph.entry(b).or_default().insert(a, 1);
    }
  }
  let nodes = graph.len();
  let size = min_cut_stoer_wagner(graph);
  ((nodes - size) * size, '🎄')
}
