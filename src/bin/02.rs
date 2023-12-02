fn parse_game(line: &str) -> Option<(usize, usize)> {
  let (game_id, game) = line.trim_start_matches("Game ").split_once(':')?;
  let (mut r, mut g, mut b, mut possible) = (0, 0, 0, true);
  for action in game.split([';', ',']) {
    let (n, color) = action.trim().split_once(' ')?;
    let n = n.parse().ok()?;
    match color.as_bytes()[0] {
      b'r' => {possible &= n <= 12; r = r.max(n)},
      b'g' => {possible &= n <= 13; g = g.max(n)},
      b'b' => {possible &= n <= 14; b = b.max(n)},
      _ => unreachable!(),
    }
  }
  Some((if possible {game_id.parse().ok()?} else {0}, r * g * b))
}

#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  input.split('\n').fold((0, 0), |(p1, p2), line| {
    let (a, b) = parse_game(line).unwrap();
    (p1 + a, p2 + b)
  })
}
