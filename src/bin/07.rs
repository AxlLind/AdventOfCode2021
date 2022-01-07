use itertools::Itertools;

fn gauss(n: i32) -> i32 { n * (n + 1) / 2 }

#[aoc::main("07")]
fn main(input: &str) -> (i32,i32) {
  let ints = input.split(',')
    .map(|s| s.parse::<i32>().unwrap())
    .collect::<Vec<_>>();
  let (&min,&max) = ints.iter()
    .minmax()
    .into_option()
    .unwrap();
  let p1 = (min..=max)
    .map(|x| ints.iter().map(|i| (x - i).abs()).sum::<i32>())
    .min()
    .unwrap();
  let p2 = (min..=max)
    .map(|x| ints.iter().map(|i| gauss((x - i).abs())).sum::<i32>())
    .min()
    .unwrap();
  (p1,p2)
}
