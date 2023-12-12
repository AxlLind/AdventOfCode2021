use hashbrown::HashMap;
use itertools::Itertools;

fn possible_ways<'a>(cache: &mut HashMap<(&'a [u8], Option<usize>, &'a [usize]), usize>, s: &'a [u8], within: Option<usize>, remaining: &'a [usize]) -> usize {
  fn dp<'a>(cache: &mut HashMap<(&'a [u8], Option<usize>, &'a [usize]), usize>, s: &'a [u8], within: Option<usize>, remaining: &'a [usize]) -> usize {
    if s.is_empty() {
      if within.is_none() && remaining.is_empty() {
        return 1;
      }
      if remaining.len() == 1 && within == Some(remaining[0]) {
        return 1;
      }
      return 0;
    }
    let possible = s.iter().filter(|&&c| c == b'#' || c == b'?').count();
    let remaining_sum = remaining.iter().sum();
    if possible + within.unwrap_or(0) < remaining_sum {
      return 0;
    }
    if within.is_some() && remaining.is_empty() {
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

  if let Some(&x) = cache.get(&(s, within, remaining)) {
    return x;
  }
  let possible = dp(cache, s, within, remaining);
  cache.insert((s, within, remaining), possible);
  possible
}

#[aoc::main(12)]
fn main(input: &str) -> (usize, usize) {
  input.split('\n').map(|l| {
    let (vents, rest) = l.split_once(' ').unwrap();
    let nums = rest.split(',').map(|w| w.parse::<usize>().unwrap()).collect::<Vec<_>>();
    let p1 = possible_ways(&mut HashMap::new(), vents.as_bytes(), None, &nums);
    let new_vents = (0..5).map(|_| vents).join("?");
    let new_nums = (0..5).flat_map(|_| &nums).copied().collect::<Vec<_>>();
    let p2 = possible_ways(&mut HashMap::new(), new_vents.as_bytes(), None, &new_nums);
    (p1,p2)
  }).fold((0,0), |(p1,p2), (a,b)| (p1+a, p2+b))
}
