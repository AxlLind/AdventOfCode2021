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
    let (r,g,b) = actions.iter().fold((0,0,0), |(r,g,b), &(n,c)| match c {
      b'r' => (r.max(n), g, b),
      b'g' => (r, g.max(n), b),
      b'b' => (r, g, b.max(n)),
      _ => unreachable!(),
    });
    r * g * b
  }).sum();

  (p1, p2)
}
