use hashbrown::HashSet;

fn board_score(draws: &[usize], b: &[Vec<usize>]) -> usize {
  b.iter().flatten().filter(|x| !draws.contains(x)).sum()
}

fn check_board(draws: &[usize], b: &[Vec<usize>]) -> Option<usize> {
  for i in 0..5 {
    if (0..5).all(|j| draws.contains(&b[i][j])) {
      return Some(board_score(draws, b) * draws.last().unwrap());
    }
    if (0..5).all(|j| draws.contains(&b[j][i])) {
      return Some(board_score(draws, b) * draws.last().unwrap());
    }
  }
  None
}

fn part1(draws: &[usize], boards: &[Vec<Vec<usize>>]) -> usize {
  for i in 5..draws.len() {
    let winner = boards.iter()
      .find_map(|b| check_board(&draws[0..i], b));
    if let Some(score) = winner {
      return score;
    }
  }
  unreachable!()
}

fn part2(draws: &[usize], boards: &[Vec<Vec<usize>>]) -> usize {
  let mut boards = boards.iter().collect::<HashSet<_>>();
  for i in 5..draws.len() {
    let winners = boards.iter()
      .filter_map(|&b| check_board(&draws[0..i], b).map(|score| (b,score)))
      .collect::<Vec<_>>();
    for (b,_) in &winners {
      boards.remove(b);
    }
    if boards.is_empty() {
      return winners[0].1;
    }
  }
  unreachable!()
}

#[aoc::main(04)]
fn main(input: &str) -> (usize,usize) {
  let mut blocks = input.split("\n\n");
  let draws = blocks.next()
    .unwrap()
    .split(',')
    .map(|s| s.parse().unwrap())
    .collect::<Vec<_>>();
  let boards = blocks
    .map(|b| b.lines()
      .map(|l| l.split_whitespace().map(|i| i.parse().unwrap()).collect())
      .collect()
    )
    .collect::<Vec<_>>();
  (part1(&draws, &boards), part2(&draws, &boards))
}
