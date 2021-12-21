use std::mem::swap;
use hashbrown::HashMap;

type Cache = HashMap<(usize,usize,usize,usize),(usize,usize)>;

fn regular_game(mut pos1: usize, mut pos2: usize) -> usize {
  let (mut s1, mut s2, mut die, mut nrolls) = (0,0,1,0);
  loop {
    for _ in 0..3 {
      pos1 += die;
      die = (die + 1) % 100;
    }
    while pos1 > 10 { pos1 -= 10 }
    s1 += pos1;
    nrolls += 3;
    if s1 >= 1000 { break; }
    swap(&mut pos1, &mut pos2);
    swap(&mut s1, &mut s2);
  }
  nrolls * s2
}

fn quantum_game(cache: &mut Cache, s1: usize, s2: usize, pos1: usize, pos2: usize) -> (usize,usize) {
  if s2 >= 21 { return (0,1); }
  if let Some(&score) = cache.get(&(s1,s2,pos1,pos2)) { return score; }

  let mut score = (0,0);
  for (die,times) in [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)] {
    let pos1 = pos1 + die - if pos1+die > 10 {10} else {0};
    let (s1,s2) = quantum_game(cache,s2,s1+pos1,pos2,pos1);
    score = (score.0+s2*times, score.1+s1*times);
  }

  cache.insert((s1,s2,pos1,pos2), score);
  score
}

aoc2021::main! {
  let (s1,s2) = quantum_game(&mut HashMap::new(),0,0,9,6);
  (regular_game(9,6), std::cmp::max(s1,s2))
}
