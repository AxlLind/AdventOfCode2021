use std::collections::VecDeque;

use hashbrown::HashSet;
use rayon::prelude::*;
use itertools::Itertools;

#[derive(Default, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct State {
  ore: i32,
  ore_robots: i32,
  clay: i32,
  clay_robots: i32,
  obsidian: i32,
  obsidian_robots: i32,
  geode: i32,
  geode_robots: i32,
}

fn earn(mut state: State) -> State {
  state.ore += state.ore_robots;
  state.clay += state.clay_robots;
  state.obsidian += state.obsidian_robots;
  state.geode += state.geode_robots;
  state
}

fn max_geodes((ore_ore_cost, clay_ore_cost, obs_ore_cost, obs_clay_cost, geo_ore_cost, geo_obs_cost): (i32,i32,i32,i32,i32,i32), time: i32) -> i32 {
  let max_ore_cost = *[ore_ore_cost, clay_ore_cost, obs_ore_cost, geo_ore_cost].iter().max().unwrap();
  let mut seen = HashSet::new();
  let mut q = VecDeque::new();
  q.push_back((0, {
    let mut start = State::default();
    start.ore_robots = 1;
    start
  }));
  let mut ans = 0;
  while let Some((len, state)) = q.pop_front() {
    if len >= time {
      ans = std::cmp::max(ans, state.geode);
      continue;
    }
    if seen.contains(&state) {
      continue;
    }
    seen.insert(state);
    if state.ore_robots < max_ore_cost && state.ore >= ore_ore_cost {
      let mut new_state = earn(state);
      new_state.ore -= ore_ore_cost;
      new_state.ore_robots += 1;
      q.push_back((len+1, new_state));
    }
    if state.clay_robots < obs_clay_cost && state.ore >= clay_ore_cost {
      let mut new_state = earn(state);
      new_state.ore -= clay_ore_cost;
      new_state.clay_robots += 1;
      q.push_back((len+1, new_state));
    }
    if state.obsidian_robots < geo_obs_cost && state.ore >= obs_ore_cost && state.clay >= obs_clay_cost {
      let mut new_state = earn(state);
      new_state.ore -= obs_ore_cost;
      new_state.clay -= obs_clay_cost;
      new_state.obsidian_robots += 1;
      q.push_back((len+1, new_state));
    }
    if state.ore >= geo_ore_cost && state.obsidian >= geo_obs_cost {
      let mut new_state = earn(state);
      new_state.ore -= geo_ore_cost;
      new_state.obsidian -= geo_obs_cost;
      new_state.geode_robots += 1;
      q.push_back((len+1, new_state));
    } else {
      q.push_back((len+1, earn(state)));
    }
  }
  ans
}

#[aoc::main(19)]
fn main(input: &str) -> (i32, i32) {
  let blueprints = input.lines().map(|l|
    l.split(|c: char| !c.is_ascii_digit()).filter(|w| !w.is_empty()).skip(1).map(|w| w.parse::<i32>().unwrap()).collect_tuple().unwrap()
  ).collect::<Vec<_>>();
  let p1 = blueprints.par_iter().enumerate().map(|(i, &b)| (i as i32 + 1) * max_geodes(b, 24)).sum();
  let p2 = blueprints.par_iter().take(3).map(|&b| max_geodes(b, 32)).product();
  (p1,p2)
}
