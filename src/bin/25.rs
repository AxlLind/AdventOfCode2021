fn todec(s: &str) -> i64 {
  s.chars().fold(0, |n, d| {
    n * 5 + match d {
      '2' => 2,
      '1' => 1,
      '0' => 0,
      '-' => -1,
      '=' => -2,
      _ => unreachable!()
    }
  })
}

fn tosnafu(n: i64) -> String {
  match n % 5 {
    0 if n == 0 => "".to_string(),
    0 => tosnafu(n / 5) + "0",
    1 => tosnafu(n / 5) + "1",
    2 => tosnafu(n / 5) + "2",
    3 => tosnafu((n + 2) / 5) + "=",
    4 => tosnafu((n + 1) / 5) + "-",
    _ => unreachable!(),
  }
}

#[aoc::main(25)]
fn main(input: &str) -> (String, char) {
  (tosnafu(input.lines().map(todec).sum()), '🎄')
}
