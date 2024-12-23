fn hash(s: &str) -> usize {
  s.bytes().fold(0, |a, c| (a + c) * 17) as usize
}

#[aoc::main(15)]
fn main(input: &str) -> (usize, usize) {
  let mut map = vec![Vec::new(); 256];
  let mut p1 = 0;
  for w in input.split(',') {
    p1 += hash(w);
    let label_end = w.bytes().position(|c| c == b'-' || c == b'=').unwrap();
    let label = &w[..label_end];
    let bucket = &mut map[hash(label)];
    match (w.as_bytes()[label_end], bucket.iter().position(|&(l,_)| l == label)) {
      (b'=', Some(i)) => bucket[i] = (label, w[label_end+1..].parse::<usize>().unwrap()),
      (b'=', None)    => bucket.push((label, w[label_end+1..].parse::<usize>().unwrap())),
      (b'-', Some(i)) => { bucket.remove(i); },
      (b'-', None)    => {},
      _ => unreachable!(),
    }
  }
  let p2 = (0..map.len())
    .flat_map(|b| (0..map[b].len()).map(move |i| (b, i)))
    .map(|(b, i)| (b + 1) * (i + 1) * map[b][i].1)
    .sum();
  (p1, p2)
}
