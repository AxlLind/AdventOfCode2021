use itertools::Itertools;

fn p1(seeds: Vec<usize>, maps: &[Vec<(usize, usize, usize)>]) -> usize {
  let ans = maps.iter().fold(seeds, |seeds, mappings|
    seeds.iter().map(|&seed|
      mappings.iter()
        .find(|&&(_, src, range)| (src..src+range).contains(&seed))
        .map(|(dst, src, _)| dst + seed - src)
        .unwrap_or(seed)
    ).collect::<Vec<_>>()
  );
  *ans.iter().min().unwrap()
}

fn p2(seeds: Vec<usize>, maps: &[Vec<(usize, usize, usize)>]) -> usize {
  let seeds = seeds.iter()
    .tuples()
    .map(|(&a, len)| (a, a+len))
    .collect::<Vec<_>>();
  let mapped_seeds = maps.iter().fold(seeds, |seeds, mappings| {
    seeds.iter().flat_map(|&(start, len)| {
      let mut mapped = Vec::new();
      let mut unmapped = vec![(start, len)];
      for &(dst, src, len) in mappings {
        let mut m = Vec::new();
        for (start, end) in unmapped {
          let a = (start, end.min(src));
          let b = (start.max(src), (src+len).min(end));
          let c = ((src+len).max(start), end);
          if a.1 > a.0 { m.push(a); }
          if b.1 > b.0 { mapped.push((b.0-src+dst, b.1-src+dst)); }
          if c.1 > c.0 { m.push(c); }
        }
        unmapped = m;
      }
      mapped.extend(unmapped);
      mapped
    }).collect::<Vec<_>>()
  });
  mapped_seeds.iter().map(|&(s, _)| s).min().unwrap()
}

#[aoc::main(05)]
fn main(input: &str) -> (usize, usize) {
  let (seeds, rest) = input.split_once("\n\n").unwrap();
  let seeds = seeds.split_whitespace()
    .skip(1)
    .map(|s| s.parse::<usize>().unwrap())
    .collect::<Vec<_>>();
  let maps = rest.split("\n\n").map(|s| {
    s.split('\n').skip(1).map(|l|
      l.split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap()
    ).collect::<Vec<_>>()
  }).collect::<Vec<_>>();
  (p1(seeds.clone(), &maps), p2(seeds, &maps))
}
