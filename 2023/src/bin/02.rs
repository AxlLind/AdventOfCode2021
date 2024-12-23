#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
  let games = input.split('\n').map(|l| {
    let (id, game) = l.trim_start_matches("Game ").split_once(':').unwrap();
    let colors = game.split([';', ',']).map(|w| {
      let (n, color) = w.trim().split_once(' ').unwrap();
      let c = "rgb".bytes().position(|c| c == color.as_bytes()[0]).unwrap();
      (n.parse().unwrap(), c)
    }).collect::<Vec<_>>();
    (id.parse().unwrap(), colors)
  }).collect::<Vec<_>>();

  let p1 = games.iter()
    .filter(|(_, actions)| actions.iter().all(|&(n,c)| n <= [12, 13, 14][c]))
    .map(|(id,_)| id)
    .sum();

  let p2 = games.iter().map(|(_,actions)| {
    let mut a = [0; 3];
    for &(n, c) in actions {
      a[c] = a[c].max(n);
    }
    a[0] * a[1] * a[2]
  }).sum();

  (p1, p2)
}
