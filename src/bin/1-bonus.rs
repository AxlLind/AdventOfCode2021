use easy_io::input_reader::InputReader;

fn main() {
  let mut input = InputReader::new();
  let mut ans = 0;
  while input.has_more() {
    let mut mass = input.next_i32();
    loop {
      mass = (mass / 3) - 2;
      if mass <= 0 { break; }
      ans += mass;
    }
  }
  println!("{}", ans);
}
