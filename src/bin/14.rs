use std::collections::HashMap;
use itertools::Itertools;

fn simulate_polymer(init: &str, recipes: &HashMap<(char,char),char>, steps: usize) -> usize {
  let init_count = init.chars().tuple_windows().counts();
  let pair_counts = (0..steps).fold(init_count, |counts, _| {
    let mut next_counts = HashMap::new();
    for (&(a,b),count) in &counts {
      let c = recipes[&(a,b)];
      *next_counts.entry((a,c)).or_insert(0) += count;
      *next_counts.entry((c,b)).or_insert(0) += count;
    }
    next_counts
  });
  let mut count = HashMap::new();
  for (&(a,_), c) in &pair_counts {
    *count.entry(a).or_insert(0) += c;
  }
  *count.entry(init.chars().last().unwrap()).or_insert(0) += 1;
  let (min,max) = count.values().minmax().into_option().unwrap();
  max - min
}

#[aoc::main(14)]
fn main(input: &str) -> (usize,usize) {
  let (init, ingredients) = input.split_once("\n\n").unwrap();
  let recipes = ingredients.lines()
    .map(|l| {
      let (a,b) = l.split_once(" -> ").unwrap();
      let k = (a.as_bytes()[0] as char, a.as_bytes()[1] as char);
      (k, b.as_bytes()[0] as char)
    })
    .collect::<HashMap<_,_>>();
  let p1 = simulate_polymer(init, &recipes, 10);
  let p2 = simulate_polymer(init, &recipes, 40);
  (p1,p2)
}
