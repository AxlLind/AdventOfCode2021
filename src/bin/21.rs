use std::mem::swap;
use hashbrown::HashMap;
use itertools::iproduct;

type Cache = HashMap<(usize,usize,usize,usize),(usize,usize)>;

fn regular_game(mut pos1: usize, mut pos2: usize) -> usize {
  let (mut p1, mut p2, mut die, mut nrolls) = (0,0,1,0);
  loop {
    for _ in 0..3 {
      pos1 += die;
      die = (die + 1) % 100;
    }
    while pos1 > 10 { pos1 -= 10 }
    p1 += pos1;
    nrolls += 3;
    if p1 >= 1000 { break; }
    swap(&mut pos1, &mut pos2);
    swap(&mut p1, &mut p2);
  }
  nrolls * p2
}

fn quantum_game(cache: &mut Cache, p1: usize, p2: usize, pos1: usize, pos2: usize) -> (usize,usize) {
  if p1 >= 21 { return (1,0); }
  if p2 >= 21 { return (0,1); }
  if let Some(&score) = cache.get(&(p1,p2,pos1,pos2)) { return score; }

  let mut score = (0,0);
  for (d1,d2,d3) in iproduct!([1,2,3],[1,2,3],[1,2,3]) {
    let die = d1+d2+d3;
    let pos1 = pos1 + die - if pos1+die > 10 {10} else {0};
    let (s1,s2) = quantum_game(cache,p2,p1+pos1,pos2,pos1);
    score.0 += s2;
    score.1 += s1;
  }

  cache.insert((p1,p2,pos1,pos2), score);
  score
}

aoc2021::main! {
  let (s1,s2) = quantum_game(&mut HashMap::new(),0,0,9,6);
  (regular_game(9,6), std::cmp::max(s1,s2))
}
