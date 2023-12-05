use itertools::Itertools;

fn p1(seeds: Vec<usize>, layers: &[Vec<(usize, usize, usize)>]) -> usize {
  let locations = layers.iter().fold(seeds, |seeds, mappings|
    seeds.iter().map(|&seed|
      mappings.iter()
        .find(|&&(_, src, range)| (src..src+range).contains(&seed))
        .map(|(dst, src, _)| dst + seed - src)
        .unwrap_or(seed)
    ).collect()
  );
  *locations.iter().min().unwrap()
}

fn p2(seeds: Vec<usize>, layers: &[Vec<(usize, usize, usize)>]) -> usize {
  let seeds = seeds.iter()
    .tuples()
    .map(|(&a, len)| (a, a + len))
    .collect::<Vec<_>>();
  let locations = layers.iter().fold(seeds, |seeds, mappings|
    seeds.iter().flat_map(|&(start, end)| {
      let mut mapped = Vec::new();
      let mut unmapped = vec![(start, end)];
      for &(dst, src, len) in mappings {
        let mut m = Vec::new();
        for (start, end) in unmapped {
          let a = (start, end.min(src));
          let b = (start.max(src), (src+len).min(end));
          let c = ((src+len).max(start), end);
          if a.0 < a.1 { m.push(a); }
          if b.0 < b.1 { mapped.push((b.0-src+dst, b.1-src+dst)); }
          if c.0 < c.1 { m.push(c); }
        }
        unmapped = m;
      }
      mapped.extend(unmapped);
      mapped
    }).collect()
  );
  locations.iter().map(|&(s, _)| s).min().unwrap()
}

#[aoc::main(05)]
fn main(input: &str) -> (usize, usize) {
  let (seeds, rest) = input.split_once("\n\n").unwrap();
  let seeds = seeds.split_whitespace()
    .skip(1)
    .map(|s| s.parse().unwrap())
    .collect::<Vec<_>>();
  let layers = rest.split("\n\n").map(|s|
    s.split('\n').skip(1).map(|l|
      l.split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect_tuple()
        .unwrap()
    ).collect::<Vec<_>>()
  ).collect::<Vec<_>>();
  (p1(seeds.clone(), &layers), p2(seeds, &layers))
}
