use std::collections::VecDeque;
use itertools::Itertools;
use hashbrown::HashSet;
use rayon::prelude::*;

#[derive(Default, Clone, Copy, Hash, PartialEq, Eq)]
struct State {
  ore: u32,
  ore_robots: u32,
  clay: u32,
  clay_robots: u32,
  obsidian: u32,
  obsidian_robots: u32,
  geode: u32,
  geode_robots: u32,
}

fn earn(mut state: State) -> State {
  state.ore += state.ore_robots;
  state.clay += state.clay_robots;
  state.obsidian += state.obsidian_robots;
  state.geode += state.geode_robots;
  state
}

fn max_geodes((ore_ore_cost, clay_ore_cost, obs_ore_cost, obs_clay_cost, geo_ore_cost, geo_obs_cost): (u32,u32,u32,u32,u32,u32), time: u32) -> u32 {
  let max_ore_cost = *[ore_ore_cost, clay_ore_cost, obs_ore_cost, geo_ore_cost].iter().max().unwrap();
  let (mut seen, mut q) = (HashSet::new(), VecDeque::new());
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
      let mut state = earn(state);
      state.ore -= ore_ore_cost;
      state.ore_robots += 1;
      q.push_back((len+1, state));
    }
    if state.clay_robots < obs_clay_cost && state.ore >= clay_ore_cost {
      let mut state = earn(state);
      state.ore -= clay_ore_cost;
      state.clay_robots += 1;
      q.push_back((len+1, state));
    }
    if state.obsidian_robots < geo_obs_cost && state.ore >= obs_ore_cost && state.clay >= obs_clay_cost {
      let mut state = earn(state);
      state.ore -= obs_ore_cost;
      state.clay -= obs_clay_cost;
      state.obsidian_robots += 1;
      q.push_back((len+1, state));
    }
    if state.ore >= geo_ore_cost && state.obsidian >= geo_obs_cost {
      let mut state = earn(state);
      state.ore -= geo_ore_cost;
      state.obsidian -= geo_obs_cost;
      state.geode_robots += 1;
      q.push_back((len+1, state));
    } else {
      q.push_back((len+1, earn(state)));
    }
  }
  ans
}

#[aoc::main(19)]
fn main(input: &str) -> (u32, u32) {
  let blueprints = input.lines().filter_map(|l|
    l.split(|c: char| !c.is_ascii_digit())
      .filter(|w| !w.is_empty())
      .skip(1)
      .map(|w| w.parse().unwrap())
      .collect_tuple()
  ).collect::<Vec<_>>();
  let p1 = blueprints.par_iter().enumerate().map(|(i,&b)| max_geodes(b, 24) * (i as u32 + 1)).sum();
  let p2 = blueprints.par_iter().take(3).map(|&b| max_geodes(b, 32)).product();
  (p1,p2)
}
