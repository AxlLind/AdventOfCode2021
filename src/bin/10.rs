use std::collections::*;
use itertools::Itertools;

static INPUT: [usize; 92] = [76,51,117,97,7,77,63,18,137,10,23,14,130,131,8,91,17,29,2,36,110,35,113,30,112,61,83,122,28,75,124,82,101,135,42,44,128,32,55,85,119,114,72,111,107,123,54,3,98,96,11,62,22,49,37,1,104,43,24,31,129,69,4,21,48,39,9,38,58,125,81,89,65,90,118,64,25,138,16,78,92,102,88,95,132,47,50,15,68,84,136,103];

fn part_one(v: &[usize]) -> usize {
  let (ones, threes) = v.iter()
    .tuple_windows()
    .fold((1,1), |(ones, threes), (a, b)| match b - a {
      1 => (ones + 1, threes),
      3 => (ones, threes + 1),
      _ => unreachable!(),
    });
  ones * threes
}

// A recursive backtracking approach for part two.
// The DP solution below is a bit simpler but I initially implemented this.
#[allow(unused)]
fn num_paths(
  cache: &mut HashMap<usize, usize>,
  v: &[usize],
  target: usize,
  i: usize
) -> usize {
  if !v.contains(&i) {
    return 0;
  }
  if i == target {
    return 1;
  }

  if !cache.contains_key(&i) {
    let ans =
      num_paths(cache, v, target, i + 1) +
      num_paths(cache, v, target, i + 2) +
      num_paths(cache, v, target, i + 3);
    cache.insert(i, ans);
  }
  cache[&i]
}

fn part_two(v: &[usize]) -> usize {
  let mut dp = HashMap::new();
  dp.insert(0, 1);
  for &i in v {
    let ans =
      dp.get(&(i - 1)).unwrap_or(&0) +
      dp.get(&(i - 2)).unwrap_or(&0) +
      dp.get(&(i - 3)).unwrap_or(&0);
    dp.insert(i, ans);
  }
  dp[v.last().unwrap()]
}

aoc2020::main! {
  let v = INPUT.iter()
    .cloned()
    .sorted()
    .collect::<Vec<_>>();
  (part_one(&v), part_two(&v))
}
