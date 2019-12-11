use std::time::Instant;

fn main() {
  let now = Instant::now();
  let answer = (206938..679128)
    .filter(|n| {
      let digits = n.to_string()
        .chars()
        .map(|c| c as u8 - b'0')
        .collect::<Vec<_>>();
      let mut repeating = false;
      for i in 1..digits.len() {
        if digits[i] < digits[i-1] { return false; }
        if digits[i] == digits[i-1] { repeating = true; }
      }
      repeating
    })
    .count();
  println!("{}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
