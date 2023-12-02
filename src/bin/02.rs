#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  input.split('\n').fold((0, 0), |(mut p1, p2), line| {
    let (game_id, rest) = line.trim().split_once(':').unwrap();
    let (mut r, mut g, mut b, mut possible) = (0, 0, 0, true);
    for game in rest.split(';') {
      for s in game.split(',') {
        let (n, color) = s.trim().split_once(' ').unwrap();
        let n = n.parse().unwrap();
        let limit = match color.as_bytes()[0] {
          b'r' => {r = r.max(n); 12},
          b'g' => {g = g.max(n); 13},
          b'b' => {b = b.max(n); 14},
          _ => unreachable!(),
        };
        possible &= n <= limit;
      }
    }
    if possible {
      p1 += game_id.split_once(' ').unwrap().1.parse::<usize>().unwrap();
    }
    (p1, p2 + r * g * b)
  })
}
