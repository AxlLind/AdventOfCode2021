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

fn get_hand_type(counts: &[usize], jokers: usize) -> usize {
  match (*counts.iter().max().unwrap_or(&0), jokers) {
    (a,b) if a + b == 5 => 6,
    (a,b) if a + b == 4 => 5,
    (3,0) => if counts.contains(&2) {4} else {3},
    (2,_) => {
      let pairs = counts.iter().filter(|&&v| v == 2).count();
      match (pairs, jokers) {
        (2,1) => 4,
        (1,1) => 3,
        (2,0) => 2,
        _ => 1,
      }
    },
    (1,2) => 3,
    (1,1) => 1,
    _ => 0,
  }
}

fn hand_strength(cards: &str, p2: bool) -> (usize, usize) {
  let counts_by_card = cards.chars().counts();
  let counts = counts_by_card.iter()
    .filter(|&(&k,_)| k != 'J' || !p2)
    .map(|(_,&v)| v)
    .collect::<Vec<_>>();
  let jokers = if p2 {*counts_by_card.get(&'J').unwrap_or(&0)} else {0};
  let idx = cards.chars().fold(0, |acc, c| (acc << 4) + card_index(c, p2));
  (get_hand_type(&counts, jokers), idx)
}

#[aoc::main(07)]
fn main(input: &str) -> (usize, usize) {
  let mut cards = input.split('\n').map(|l| {
    let (cards, bid) = l.split_once(' ').unwrap();
    let p1key = hand_strength(cards, false);
    let p2key = hand_strength(cards, true);
    (cards, bid.parse().unwrap(), p1key, p2key)
  }).collect::<Vec<_>>();
  cards.sort_unstable_by_key(|&(_,_,key,_)| key);
  let p1 = cards.iter().enumerate().map(|(i, (_,bid,_,_))| (i + 1) * bid).sum();
  cards.sort_unstable_by_key(|&(_,_,_,key)| key);
  let p2 = cards.iter().enumerate().map(|(i, (_,bid,_,_))| (i + 1) * bid).sum();
  (p1, p2)
}
