#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  let games = input.split('\n').map(|l| {
    let (game_id, game) = l.trim_start_matches("Game ").split_once(':').unwrap();
    let colors = game.split([';', ',']).map(|w| {
      let (n, color) = w.trim().split_once(' ').unwrap();
      (n.parse().unwrap(), color.as_bytes()[0])
    }).collect::<Vec<_>>();
    (game_id.parse().unwrap(), colors)
  }).collect::<Vec<_>>();

  let p1 = games.iter().filter(|(_, actions)| {
    actions.iter().all(|&(n,c)| match c {
      b'r' => n <= 12,
      b'g' => n <= 13,
      b'b' => n <= 14,
      _ => unreachable!(),
    })
  }).map(|(id,_)| id).sum();

  let p2 = games.iter().map(|(_,actions)| {
    let (mut r, mut g, mut b) = (0,0,0);
    for &(n, c) in actions {
      match c {
        b'r' => r = r.max(n),
        b'g' => g = g.max(n),
        b'b' => b = b.max(n),
        _ => unreachable!(),
      }
    }
    r * g * b
  }).sum();

  (p1, p2)
}
