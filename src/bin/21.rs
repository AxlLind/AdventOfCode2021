use hashbrown::HashMap;
use itertools::iproduct;

type Cache = HashMap<(usize,usize,usize,usize,bool),(usize,usize)>;

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
    if p1 >= 1000 { break }

    for _ in 0..3 {
      pos2 += die;
      die = (die + 1) % 100;
    }
    while pos2 > 10 { pos2 -= 10 }
    p2 += pos2;
    nrolls += 3;
    if p2 >= 1000 { break }
  }
  nrolls * if p1 >= 1000 {p2} else {p1}
}

fn quantum_game(cache: &mut Cache, p1: usize, p2: usize, pos1: usize, pos2: usize, turn1: bool) -> (usize,usize) {
  if p1 >= 21 { return (1,0); }
  if p2 >= 21 { return (0,1); }
  if let Some(&score) = cache.get(&(p1,p2,pos1,pos2,turn1))  { return score; }
  if let Some(&score) = cache.get(&(p2,p1,pos2,pos1,!turn1)) { return (score.1,score.0); }

  let mut score = (0,0);
  for (d1,d2,d3) in iproduct!([1,2,3],[1,2,3],[1,2,3]) {
    let die = d1+d2+d3;
    let (s1,s2) = if turn1 {
      let pos1 = pos1 + die - if pos1+die > 10 {10} else {0};
      quantum_game(cache,p1+pos1,p2,pos1,pos2,false)
    } else {
      let pos2 = pos2 + die - if pos2+die > 10 {10} else {0};
      quantum_game(cache,p1,p2+pos2,pos1,pos2,true)
    };
    score.0 += s1;
    score.1 += s2;
  }

  cache.insert((p1,p2,pos1,pos2,turn1), score);
  score
}

aoc2021::main! {
  let p1 = regular_game(9,6);
  let (p2,_) = quantum_game(&mut HashMap::new(),0,0,9,6,true);
  (p1,p2)
}
