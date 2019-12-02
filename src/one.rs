mod input_reader;

use input_reader::InputReader;

fn main() {
  let mut input = InputReader::new();
  let mut ans = 0;
  while input.has_more() {
    ans += input.next_i32() / 3 - 2;
  }
  println!("{}", ans);
}
