const A: usize = 13316116;
const B: usize = 13651422;

aoc2020::main! {
  let (mut x, mut loop_size) = (1,0);
  while x != A && x != B {
    x = x * 7 % 20201227;
    loop_size += 1;
  }
  let y = if x == A {B} else {A};
  let key = (0..loop_size).fold(1, |x,_| x * y % 20201227);
  (key, '🎄')
}
