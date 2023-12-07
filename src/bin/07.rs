use std::cmp::Ordering;
use itertools::Itertools;

fn card_index(c: char, p2: bool) -> usize {
  match c {
    'A' => 14,
    'K' => 13,
    'Q' => 12,
    'J' => if p2 {0} else {11},
    'T' => 10,
    '9' => 9,
    '8' => 8,
    '7' => 7,
    '6' => 6,
    '5' => 5,
    '4' => 4,
    '3' => 3,
    '2' => 2,
    _ => unreachable!()
  }
}

fn card_type(cards: &str, p2: bool) -> usize {
  let counts_by_card = cards.chars().counts();
  let counts = counts_by_card.iter()
    .filter(|&(k,_)| *k != 'J' || !p2)
    .map(|(_,v)| *v)
    .collect::<Vec<_>>();
  let jokers = if p2 {*counts_by_card.get(&'J').unwrap_or(&0)} else {0};
  match (*counts.iter().max().unwrap_or(&0), jokers) {
    (a,b) if a + b == 5 => 6,
    (a,b) if a + b == 4 => 5,
    (3,0) => if counts.contains(&2) {4} else {3},
    (2,_) => {
      let pairs = counts.iter().filter(|&&v| v == 2).count();
      match (pairs, jokers) {
        (2,1) => 4,
        (2,0) => 2,
        (1,1) => 3,
        _ => 1,
      }
    },
    (1,2) => 3,
    (1,1) => 1,
    _ => 0,
  }
}

fn comp_cards(c1: &str, c2: &str, p2: bool) -> Ordering {
  match card_type(c1, p2).cmp(&card_type(c2, p2)) {
    Ordering::Equal => {
      c1.chars().zip(c2.chars()).find_map(|(a,b)| {
        match card_index(a, p2).cmp(&card_index(b, p2)) {
          Ordering::Equal => None,
          ord => Some(ord),
        }
      }).unwrap()
    },
    ord => ord,
  }
}

#[aoc::main(07)]
fn main(input: &str) -> (usize, usize) {
  let mut cards = input.split('\n').map(|l| {
    let (cards, n) = l.split_once(' ').unwrap();
    (cards, n.parse().unwrap())
  }).collect::<Vec<_>>();
  cards.sort_by(|a, b| comp_cards(a.0, b.0, false));
  let p1 = cards.iter().enumerate().map(|(rank, (_, bid))| (rank + 1) * bid).sum();
  cards.sort_by(|a, b| comp_cards(a.0, b.0, true));
  let p2 = cards.iter().enumerate().map(|(rank, (_, bid))| (rank + 1) * bid).sum();
  (p1, p2)
}
