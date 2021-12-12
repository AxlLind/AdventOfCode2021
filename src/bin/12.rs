use std::collections::{HashMap, HashSet};

static INPUT: &str = "ex-NL\nex-um\nql-wv\nVF-fo\nVF-ql\nstart-VF\nend-tg\nwv-ZQ\nwv-um\nNL-start\nlx-ex\nex-wv\nex-fo\nsb-start\num-end\nfo-ql\nNL-sb\nNL-fo\ntg-NL\nVF-sb\nfo-wv\nex-VF\nql-sb\nend-wv";

fn part1<'a>(graph: &HashMap<&'a str, Vec<&'a str>>, src: &'a str, seen: &mut HashSet<&'a str>) -> usize {
  if src == "end" {
    return 1;
  }
  if src.chars().all(|c| c.is_lowercase()) && !seen.insert(src) {
    return 0;
  }
  let ans = graph[src].iter()
    .map(|n| part1(graph, n, seen))
    .sum::<usize>();
  seen.remove(src);
  ans
}

fn part2<'a>(graph: &HashMap<&'a str, Vec<&'a str>>, src: &'a str, seen: &mut HashSet<&'a str>, mut seen_twice: Option<&'a str>) -> usize {
  if src == "end" {
    return 1;
  }
  if src.chars().all(|c| c.is_lowercase()) && !seen.insert(src) {
    if seen_twice.is_some() || src == "start" {
      return 0;
    }
    seen_twice = Some(src);
  }
  let ans = graph[src].iter()
    .map(|n| part2(graph, n, seen, seen_twice))
    .sum::<usize>();
  if seen_twice.unwrap_or("") != src {
    seen.remove(src);
  }
  ans
}

aoc2021::main! {
  let mut graph = HashMap::new();
  for l in INPUT.lines() {
    let (a,b) = l.split_once('-').unwrap();
    graph.entry(a).or_insert(Vec::new()).push(b);
    graph.entry(b).or_insert(Vec::new()).push(a);
  }
  let p1 = part1(&graph, "start", &mut HashSet::new());
  let p2 = part2(&graph, "start", &mut HashSet::new(), None);
  (p1,p2)
}
