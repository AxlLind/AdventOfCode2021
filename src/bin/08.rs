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

fn steps(path: &[u8], graph: &HashMap<&[u8],(&[u8],&[u8])>, start: &[u8], p2: bool) -> usize {
  let mut t = 0;
  let mut node = start;
  loop {
    let left = path[t % path.len()] == b'L';
    node = if left {graph[node].0} else {graph[node].1};
    if node.ends_with(if p2 {b"Z"} else {b"ZZZ"}) {
      return t+1;
    }
    t += 1;
  }
}
fn p2(path: &[u8], graph: &HashMap<&[u8],(&[u8],&[u8])>) -> usize {
  let num_steps = graph.keys()
    .filter(|k| k.ends_with(b"A"))
    .map(|node| steps(path, graph, node, true));
  lcm(num_steps)
}

#[aoc::main(08)]
fn main(input: &str) -> (usize, usize) {
  let (path, rest) = input.split_once("\n\n").unwrap();
  let graph = rest.split('\n').map(|l| {
    let l = l.as_bytes();
    (&l[0..3], (&l[7..10], &l[12..15]))
  }).collect::<HashMap<_,_>>();
  (steps(path.as_bytes(), &graph, b"AAA", false), p2(path.as_bytes(), &graph))
}
