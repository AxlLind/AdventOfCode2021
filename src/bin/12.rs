use hashbrown::HashMap;

fn num_paths<'a>(
  graph: &HashMap<&'a str, Vec<&'a str>>,
  src: &'a str,
  path: &mut Vec<&'a str>,
  mut seen_twice: bool
) -> usize {
  if src == "end" {
    return 1;
  }
  if src.chars().all(|c| c.is_lowercase()) && path.contains(&src) {
    if seen_twice || src == "start" {
      return 0;
    }
    seen_twice = true;
  }
  path.push(src);
  let ans = graph[src].iter()
    .map(|n| num_paths(graph, n, path, seen_twice))
    .sum();
  path.pop();
  ans
}

#[aoc::main("12")]
fn main(input: &str) -> (usize,usize) {
  let mut graph = HashMap::new();
  for l in input.lines() {
    let (a,b) = l.split_once('-').unwrap();
    graph.entry(a).or_insert(Vec::new()).push(b);
    graph.entry(b).or_insert(Vec::new()).push(a);
  }
  let p1 = num_paths(&graph, "start", &mut Vec::new(), true);
  let p2 = num_paths(&graph, "start", &mut Vec::new(), false);
  (p1,p2)
}
