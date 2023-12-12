use hashbrown::HashMap;
use itertools::Itertools;

fn possible_ways(cache: &mut HashMap<(usize, usize, usize), usize>, s: &[u8], within: Option<usize>, remaining: &[usize]) -> usize {
  fn dp(cache: &mut HashMap<(usize, usize, usize), usize>, s: &[u8], within: Option<usize>, remaining: &[usize]) -> usize {
    if s.is_empty() {
      return match (within, remaining.len()) {
        (None, 0) => 1,
        (Some(x), 1) if x == remaining[0] => 1,
        _ => 0
      };
    }
    let possible = s.iter().filter(|&&c| c == b'#' || c == b'?').count();
    let remaining_sum = remaining.iter().sum();
    if possible + within.unwrap_or(0) < remaining_sum {
      return 0;
    }
    if within.is_some() && remaining_sum == 0 {
      return 0;
    }
    if s[0] == b'.' && within.is_some_and(|x| x != remaining[0]) {
      return 0;
    }
    let mut possible = 0;
    if matches!((s[0], within), (b'.', Some(_))) {
      possible += possible_ways(cache, &s[1..], None, &remaining[1..]);
    }
    if matches!((s[0], within), (b'?', Some(x)) if x == remaining[0]) {
      possible += possible_ways(cache, &s[1..], None, &remaining[1..]);
    }
    if matches!((s[0], within), (b'#' | b'?', Some(_))) {
      possible += possible_ways(cache, &s[1..], within.map(|x| x+1), remaining);
    }
    if matches!((s[0], within), (b'#' | b'?', None)) {
      possible += possible_ways(cache, &s[1..], Some(1), remaining);
    }
    if matches!((s[0], within), (b'.' | b'?', None)) {
      possible += possible_ways(cache, &s[1..], None, remaining);
    }
    possible
  }

  let key = (s.len(), within.unwrap_or(0), remaining.len());
  if let Some(&x) = cache.get(&key) {
    return x;
  }
  let possible = dp(cache, s, within, remaining);
  cache.insert(key, possible);
  possible
}

#[aoc::main(12)]
fn main(input: &str) -> (usize, usize) {
  let mut cache_p1 = HashMap::new();
  let mut cache_p2 = HashMap::new();
  input.split('\n').map(|l| {
    let (vents, rest) = l.split_once(' ').unwrap();
    let nums = rest.split(',').map(|w| w.parse::<usize>().unwrap()).collect::<Vec<_>>();
    cache_p1.clear();
    cache_p2.clear();
    let p1 = possible_ways(&mut cache_p1, vents.as_bytes(), None, &nums);
    let new_vents = (0..5).map(|_| vents).join("?");
    let new_nums = (0..5).flat_map(|_| &nums).copied().collect::<Vec<_>>();
    let p2 = possible_ways(&mut cache_p2, new_vents.as_bytes(), None, &new_nums);
    (p1,p2)
  }).fold((0,0), |(p1,p2), (a,b)| (p1+a, p2+b))
}
