use itertools::Itertools;
use hashbrown::HashSet;
use rayon::prelude::*;

#[derive(Default, Clone, Copy, Hash, PartialEq, Eq)]
struct State {
  ore: u16,
  ore_robots: u16,
  clay: u16,
  clay_robots: u16,
  obsidian: u16,
  obsidian_robots: u16,
  geode: u16,
  geode_robots: u16,
}

fn earn(state: State, max_ore_cost: u16, max_clay_cost: u16, max_obs_cost: u16) -> State {
  State {
    ore: (state.ore + state.ore_robots).min(3 * max_ore_cost),
    clay: (state.clay + state.clay_robots).min(3 * max_clay_cost),
    obsidian: (state.obsidian + state.obsidian_robots).min(3 * max_obs_cost),
    geode: state.geode + state.geode_robots,
    ..state
  }
}

fn max_geodes((ore_ore_cost, clay_ore_cost, obs_ore_cost, obs_clay_cost, geo_ore_cost, geo_obs_cost): (u16,u16,u16,u16,u16,u16), time: u16) -> u16 {
  let max_ore_cost = *[ore_ore_cost, clay_ore_cost, obs_ore_cost, geo_ore_cost].iter().max().unwrap();
  let mut seen = HashSet::with_capacity(100000);
  let mut states = vec![State { ore_robots: 1, ..State::default() }];
  let mut best = 0;
  for t in 0..time {
    let mut new_states = vec![];
    for &state in &states {
      if state.geode + state.geode_robots * 2 * (time - 1 - t) < best {
        continue;
      }
      if !seen.insert(state) {
        continue;
      }
      best = best.max(state.geode);
      if state.ore >= geo_ore_cost && state.obsidian >= geo_obs_cost {
        let mut state = earn(state, max_ore_cost, obs_clay_cost, geo_obs_cost);
        state.ore -= geo_ore_cost;
        state.obsidian -= geo_obs_cost;
        state.geode_robots += 1;
        new_states.push(state);
        continue;
      }
      if state.obsidian_robots < geo_obs_cost && state.ore >= obs_ore_cost && state.clay >= obs_clay_cost {
        let mut state = earn(state, max_ore_cost, obs_clay_cost, geo_obs_cost);
        state.ore -= obs_ore_cost;
        state.clay -= obs_clay_cost;
        state.obsidian_robots += 1;
        new_states.push(state);
        continue;
      }
      if state.ore_robots < max_ore_cost && state.ore >= ore_ore_cost {
        let mut state = earn(state, max_ore_cost, obs_clay_cost, geo_obs_cost);
        state.ore -= ore_ore_cost;
        state.ore_robots += 1;
        new_states.push(state);
      }
      if state.clay_robots < obs_clay_cost && state.ore >= clay_ore_cost {
        let mut state = earn(state, max_ore_cost, obs_clay_cost, geo_obs_cost);
        state.ore -= clay_ore_cost;
        state.clay_robots += 1;
        new_states.push(state);
      }
      new_states.push(earn(state, max_ore_cost, obs_clay_cost, geo_obs_cost));
    }
    states = new_states;
  }
  states.iter().map(|state| state.geode).max().unwrap()
}

#[aoc::main(19)]
fn main(input: &str) -> (u16, u16) {
  let blueprints = input.lines().filter_map(|l|
    l.split(|c: char| !c.is_ascii_digit())
      .filter(|w| !w.is_empty())
      .skip(1)
      .map(|w| w.parse().unwrap())
      .collect_tuple()
  ).collect::<Vec<_>>();
  let p1 = blueprints.par_iter().enumerate().map(|(i,&b)| max_geodes(b, 24) * (i + 1) as u16).sum();
  let p2 = blueprints.par_iter().take(3).map(|&b| max_geodes(b, 32)).product();
  (p1,p2)
}
