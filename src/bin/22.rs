use std::collections::*;

static PLAYER1: [usize; 25] = [45,10,43,46,25,36,16,38,30,15,26,34,9,2,44,1,4,40,5,24,49,3,41,19,13];
static PLAYER2: [usize; 25] = [28,50,37,20,6,42,32,47,39,22,14,7,21,17,27,8,48,11,23,12,18,35,29,33,31];

type Deck = VecDeque<usize>;

fn score(deck: &Deck) -> usize {
  deck.iter()
    .rev()
    .enumerate()
    .map(|(i,n)| (i+1) * n)
    .sum()
}

fn part_one() -> usize {
  let mut p1 = PLAYER1.iter().copied().collect::<Deck>();
  let mut p2 = PLAYER2.iter().copied().collect::<Deck>();
  while !p1.is_empty() && !p2.is_empty() {
    let (c1,c2) = (p1.pop_front().unwrap(), p2.pop_front().unwrap());
    if c1 > c2 {
      p1.push_back(c1);
      p1.push_back(c2);
    } else {
      p2.push_back(c2);
      p2.push_back(c1);
    }
  }
  score(if p2.is_empty() {&p1} else {&p2})
}

fn game(mut p1: Deck, mut p2: Deck) -> (usize, Deck) {
  let mut prev_games = HashSet::new();
  while !p1.is_empty() && !p2.is_empty() {
    if !prev_games.insert((p1.clone(), p2.clone())) {
      return (1, p1);
    }

    let (c1,c2) = (p1.pop_front().unwrap(), p2.pop_front().unwrap());
    let winner = if c1 <= p1.len() && c2 <= p2.len() {
      let p1 = p1.iter().take(c1).copied().collect();
      let p2 = p2.iter().take(c2).copied().collect();
      game(p1,p2).0
    } else {
      if c1 > c2 {1} else {2}
    };

    if winner == 1 {
      p1.push_back(c1);
      p1.push_back(c2);
    } else {
      p2.push_back(c2);
      p2.push_back(c1);
    }
  }
  if p2.is_empty() {(1, p1)} else {(2, p2)}
}

fn part_two() -> usize {
  let p1 = PLAYER1.iter().copied().collect();
  let p2 = PLAYER2.iter().copied().collect();
  let (_,winner) = game(p1, p2);
  score(&winner)
}

aoc2020::main! {
  (part_one(), part_two())
}
