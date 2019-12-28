use std::time::Instant;
use mod_exp::mod_exp;

#[derive(Clone, Copy)]
enum Cmd { Stack, Cut(i128), Deal(i128) }

const INPUT: [Cmd; 100] = [Cmd::Deal(34), Cmd::Cut(9781), Cmd::Deal(20), Cmd::Cut(8981), Cmd::Deal(11), Cmd::Cut(-3391), Cmd::Deal(15), Cmd::Cut(1485), Cmd::Deal(10), Cmd::Cut(4826), Cmd::Stack, Cmd::Cut(1026), Cmd::Deal(30), Cmd::Cut(1354), Cmd::Deal(46), Cmd::Cut(1955), Cmd::Deal(19), Cmd::Cut(1359), Cmd::Deal(22), Cmd::Cut(9483), Cmd::Deal(52), Cmd::Cut(-2090), Cmd::Deal(50), Cmd::Stack, Cmd::Cut(-2205), Cmd::Deal(69), Cmd::Cut(-7934), Cmd::Deal(11), Cmd::Cut(8311), Cmd::Deal(42), Cmd::Cut(-5430), Cmd::Deal(57), Cmd::Stack, Cmd::Cut(-2616), Cmd::Deal(22), Cmd::Stack, Cmd::Cut(3540), Cmd::Deal(38), Cmd::Cut(-9097), Cmd::Deal(37), Cmd::Cut(-7014), Cmd::Deal(26), Cmd::Cut(6983), Cmd::Deal(11), Cmd::Stack, Cmd::Cut(-4825), Cmd::Stack, Cmd::Cut(-5791), Cmd::Deal(19), Cmd::Cut(-3577), Cmd::Deal(6), Cmd::Stack, Cmd::Deal(29), Cmd::Cut(7299), Cmd::Deal(75), Cmd::Cut(-8498), Cmd::Deal(21), Cmd::Cut(5748), Cmd::Deal(63), Cmd::Cut(-344), Cmd::Deal(5), Cmd::Cut(-4306), Cmd::Deal(65), Cmd::Cut(9431), Cmd::Deal(7), Cmd::Cut(6825), Cmd::Deal(28), Cmd::Stack, Cmd::Deal(66), Cmd::Cut(-1421), Cmd::Deal(19), Cmd::Cut(-8965), Cmd::Deal(48), Cmd::Cut(-5780), Cmd::Deal(75), Cmd::Cut(-3280), Cmd::Deal(50), Cmd::Cut(6866), Cmd::Deal(72), Cmd::Cut(-5471), Cmd::Deal(49), Cmd::Cut(-8247), Cmd::Deal(65), Cmd::Cut(3056), Cmd::Stack, Cmd::Deal(39), Cmd::Cut(7011), Cmd::Deal(48), Cmd::Cut(-9660), Cmd::Deal(56), Cmd::Cut(-6843), Cmd::Stack, Cmd::Cut(5111), Cmd::Deal(29), Cmd::Cut(-7700), Cmd::Stack, Cmd::Deal(23), Cmd::Cut(-5263), Cmd::Deal(61), Cmd::Stack];

fn part_one() -> i128 {
  const LEN: i128 = 10007;
  INPUT.iter().fold(2019, |pos, &cmd| match cmd {
    Cmd::Stack   => LEN - 1 - pos,
    Cmd::Cut(n)  => (pos - n) % LEN,
    Cmd::Deal(n) => (pos * n) % LEN,
  })
}

fn part_two() -> i128 {
  const M: i128 = 119_315_717_514_047;
  const N: i128 = 101_741_582_076_661;

  // Convert the whole process to a linear equation: ax + b
  let (a,b) = INPUT.iter().rev().fold((1,0), |(a,b), &cmd| {
    let (a_new, b_new) = match cmd {
      Cmd::Stack   => (-a, -b - 1),
      Cmd::Cut(n)  => ( a,  b + n),
      Cmd::Deal(n) => {
        let n = mod_exp(n, M-2, M);
        (a * n, b * n)
      }
    };
    (a_new % M, b_new % M)
  });

  // Applying the function n times simplifies to:
  // x * a^n + b * (a^n - 1) / (a-1)
  let term1 = 2020 * mod_exp(a,N,M) % M;
  let tmp = (mod_exp(a,N,M) - 1) * mod_exp(a-1, M-2, M) % M;
  let term2 = b * tmp % M;
  (term1 + term2) % M
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
