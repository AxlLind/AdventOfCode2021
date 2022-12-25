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
  if n == 0 {"".to_string()} else {tosnafu((n+2)/5) + ["0","1","2","=","-"][n as usize % 5]}
}

#[aoc::main(25)]
fn main(input: &str) -> (String, char) {
  (tosnafu(input.lines().map(todec).sum()), '🎄')
}
