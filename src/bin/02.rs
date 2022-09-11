use itertools::Itertools;

#[aoc::main(02)]
fn main(input: &str) -> (usize,usize) {
  let ops = input.split_whitespace()
    .tuples()
    .map(|(op,i)| (op.as_bytes()[0], i.parse().unwrap()))
    .collect::<Vec<_>>();
  let (x,y,z) = ops.iter()
    .fold((0,0,0), |(x,y,aim),(op,i)| match op {
      b'f' => (x+i, y+aim*i, aim),
      b'd' => (x, y, aim+i),
      b'u' => (x, y, aim-i),
      _ => unreachable!()
    });
  (x*z, x*y)
}
