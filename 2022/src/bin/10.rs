#[aoc::main(10)]
fn main(input: &str) -> (i32, String) {
  let (mut x, mut cycle, mut p1, mut p2) = (1i32, 0i32, 0, String::with_capacity(256));
  macro_rules! tick {
    () => {
      if cycle % 40 == 0 { p2.push('\n'); }
      p2.push(if (x - cycle % 40).abs() < 2 {'█'} else {' '});
      cycle += 1;
      if (cycle - 20) % 40 == 0 { p1 += cycle * x }
    };
  }
  for l in input.lines() {
    tick!();
    if l.len() > 4 {
      tick!();
      x += l[5..].parse::<i32>().unwrap();
    }
  }
  (p1, p2)
}
