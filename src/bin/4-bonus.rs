use itertools::Itertools;

fn main() {
  let answer = (206938..679128)
    .filter(|n| {
      let digits = n.to_string()
        .chars()
        .map(|c| c as u8 - b'0')
        .collect::<Vec<_>>();
      if (1..digits.len()).any(|i| digits[i] < digits[i-1]) {
        return false;
      }
      digits.iter()
        .group_by(|d| *d)
        .into_iter()
        .any(|(_,x)| x.count() == 2)
    })
    .count();
  println!("{}", answer);
}
