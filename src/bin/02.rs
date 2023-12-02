#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  input.split('\n')
    .filter_map(|line| {
      let (game_id, rest) = line.trim_start_matches("Game ").split_once(':')?;
      let (mut r, mut g, mut b, mut possible) = (0, 0, 0, true);
      for game in rest.split(';') {
        for s in game.split(',') {
          let (n, color) = s.trim().split_once(' ')?;
          let n = n.parse().ok()?;
          match color.as_bytes()[0] {
            b'r' => {possible &= n <= 12; r = r.max(n)},
            b'g' => {possible &= n <= 13; g = g.max(n)},
            b'b' => {possible &= n <= 14; b = b.max(n)},
            _ => unreachable!(),
          }
        }
      }
      Some((if possible {game_id.parse().ok()?} else {0}, r * g * b))
    })
    .fold((0,0), |(p1, p2), (a, b)| (p1 + a, p2 + b))
}
