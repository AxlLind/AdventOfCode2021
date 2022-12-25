fn todec(s: &str) -> usize {
  s.chars().fold(0, |n, d| n * 5 + "=-012".chars().position(|x| x == d).unwrap() - 2)
}

fn tosnafu(n: usize) -> String {
  if n == 0 {"".to_string()} else {tosnafu((n+2)/5) + ["0","1","2","=","-"][n % 5]}
}

#[aoc::main(25)]
fn main(input: &str) -> (String, char) {
  (tosnafu(input.lines().map(todec).sum()), '🎄')
}
