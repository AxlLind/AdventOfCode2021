use itertools::Itertools;

#[aoc::main("01")]
fn main(input: &str) -> (usize,usize) {
  let nums = input.lines()
    .map(|s| s.parse::<i32>().unwrap())
    .collect::<Vec<_>>();
  let p1 = nums.iter().tuple_windows().filter(|(a,b)| a < b).count();
  let p2 = nums.iter().tuple_windows().filter(|(a,_,_,d)| a < d).count();
  (p1,p2)
}
