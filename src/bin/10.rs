#[aoc::main(10)]
fn main(input: &str) -> (i32, String) {
  let instructions = input.lines()
    .map(|l| l.split_once(' ').map(|(_,i)| i.parse::<i32>().unwrap()))
    .collect::<Vec<_>>();
  let mut insts = instructions.iter().cycle();
  let (mut x, mut add) = (1, None);
  let (mut p1, mut p2) = (0, String::new());
  for cycle in 1..240 {
    if [20, 60, 100, 140, 180, 220].contains(&cycle) {
      p1 += cycle as i32 * x;
    }

    let y = (cycle - 1) % 40;
    if y == 0 { p2.push('\n'); }
    p2.push(if (x - y).abs() < 2 {'█'} else {' '});

    match add.take() {
      Some(v) => x += v,
      None => add = insts.next().copied().unwrap(),
    }
  }
  (p1, p2)
}
