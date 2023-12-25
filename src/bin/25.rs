use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn component_size(graph: &HashMap<&str, HashSet<&str>>, a: &str) -> usize {
  let mut seen = HashSet::new();
  let mut s = vec![a];
  while let Some(x) = s.pop() {
    if !seen.insert(x) {
      continue;
    }
    for b in &graph[x] {
      if !seen.contains(b) {
        s.push(b);
      }
    }
  }
  seen.len()
}

#[aoc::main(25)]
fn main(input: &str) -> (usize, char) {
  let mut graph = HashMap::<_,HashSet<_>>::new();
  let mut edges = HashSet::new();
  for l in input.split('\n') {
    let (a, rest) = l.split_once(": ").unwrap();
    for b in rest.split(' ') {
      graph.entry(a).or_default().insert(b);
      graph.entry(b).or_default().insert(a);
      let (x,y) = if a < b {(a,b)} else {(b,a)};
      edges.insert((x,y));
    }
  }
  let mut dot = String::from("graph {\n");
  for (a,b) in &edges {
    dot += &format!("{} -- {}\n", a, b);
  }
  dot += "}";
  std::fs::write("out.dot", dot).unwrap();
  println!("Run the following to visualize the graph:");
  println!("dot -Tsvg -Kneato out.dot > out.svg");
  println!("Manually find the three edges.");
  for (a,b) in [("qdp", "jxx"), ("zbr", "vsx"), ("mlp", "qqq")] {
    graph.get_mut(a).unwrap().remove(b);
    graph.get_mut(b).unwrap().remove(a);
  }
  let size = component_size(&graph, "qqq");
  ((graph.len() - size) * size, '🎄')
}
