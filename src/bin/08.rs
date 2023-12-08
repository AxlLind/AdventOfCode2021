use hashbrown::HashMap;

fn gcd(a: usize, b: usize) -> usize {
  match ((a, b), (a & 1, b & 1)) {
      ((x, y), _) if x == y => y,
      ((0, x), _) | ((x, 0), _) => x,
      ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
      ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
      ((x, y), (1, 1)) => {
          let (x, y) = (x.min(y), x.max(y));
          gcd((y - x) >> 1, x)
      }
      _ => unreachable!(),
  }
}

fn lcm(xs: impl Iterator<Item=usize>) -> usize {
  xs.fold(1, |ans, x| (x*ans) / gcd(x,ans))
}

fn solve(path: &[u8], graph: &HashMap<&[u8],(&[u8],&[u8])>, p2: bool) -> usize {
  let mut nodes = graph.keys().filter(|k| k.ends_with(if p2 {b"A"} else {b"AAA"})).copied().collect::<Vec<_>>();
  let mut ends = HashMap::new();
  let mut t = 0;
  loop {
    let left = path[t % path.len()] == b'L';
    for (i, node) in nodes.iter_mut().enumerate() {
      *node = if left {graph[node].0} else {graph[node].1};
      if node.ends_with(if p2 {b"Z"} else {b"ZZZ"}) {
        ends.entry(i).or_insert(t+1);
      }
    }
    if ends.len() == nodes.len() {
      return lcm(ends.values().copied());
    }
    t += 1;
  }
}

#[aoc::main(08)]
fn main(input: &str) -> (usize, usize) {
  let (path, rest) = input.split_once("\n\n").unwrap();
  let graph = rest.split('\n').map(|l| {
    let l = l.as_bytes();
    (&l[0..3], (&l[7..10], &l[12..15]))
  }).collect::<HashMap<_,_>>();

  (solve(path.as_bytes(), &graph, false), solve(path.as_bytes(), &graph, true))
}
