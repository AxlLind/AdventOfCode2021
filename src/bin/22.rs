use std::time::Instant;

#[derive(Clone, Copy)]
enum Cmd { Deal, Cut, Stack }

static INPUT: [(Cmd, i64); 100] = [(Cmd::Deal, 34), (Cmd::Cut, 9781), (Cmd::Deal, 20), (Cmd::Cut, 8981), (Cmd::Deal, 11), (Cmd::Cut, -3391), (Cmd::Deal, 15), (Cmd::Cut, 1485), (Cmd::Deal, 10), (Cmd::Cut, 4826), (Cmd::Stack, 0), (Cmd::Cut, 1026), (Cmd::Deal, 30), (Cmd::Cut, 1354), (Cmd::Deal, 46), (Cmd::Cut, 1955), (Cmd::Deal, 19), (Cmd::Cut, 1359), (Cmd::Deal, 22), (Cmd::Cut, 9483), (Cmd::Deal, 52), (Cmd::Cut, -2090), (Cmd::Deal, 50), (Cmd::Stack, 0), (Cmd::Cut, -2205), (Cmd::Deal, 69), (Cmd::Cut, -7934), (Cmd::Deal, 11), (Cmd::Cut, 8311), (Cmd::Deal, 42), (Cmd::Cut, -5430), (Cmd::Deal, 57), (Cmd::Stack, 0), (Cmd::Cut, -2616), (Cmd::Deal, 22), (Cmd::Stack, 0), (Cmd::Cut, 3540), (Cmd::Deal, 38), (Cmd::Cut, -9097), (Cmd::Deal, 37), (Cmd::Cut, -7014), (Cmd::Deal, 26), (Cmd::Cut, 6983), (Cmd::Deal, 11), (Cmd::Stack, 0), (Cmd::Cut, -4825), (Cmd::Stack, 0), (Cmd::Cut, -5791), (Cmd::Deal, 19), (Cmd::Cut, -3577), (Cmd::Deal, 6), (Cmd::Stack, 0), (Cmd::Deal, 29), (Cmd::Cut, 7299), (Cmd::Deal, 75), (Cmd::Cut, -8498), (Cmd::Deal, 21), (Cmd::Cut, 5748), (Cmd::Deal, 63), (Cmd::Cut, -344), (Cmd::Deal, 5), (Cmd::Cut, -4306), (Cmd::Deal, 65), (Cmd::Cut, 9431), (Cmd::Deal, 7), (Cmd::Cut, 6825), (Cmd::Deal, 28), (Cmd::Stack, 0), (Cmd::Deal, 66), (Cmd::Cut, -1421), (Cmd::Deal, 19), (Cmd::Cut, -8965), (Cmd::Deal, 48), (Cmd::Cut, -5780), (Cmd::Deal, 75), (Cmd::Cut, -3280), (Cmd::Deal, 50), (Cmd::Cut, 6866), (Cmd::Deal, 72), (Cmd::Cut, -5471), (Cmd::Deal, 49), (Cmd::Cut, -8247), (Cmd::Deal, 65), (Cmd::Cut, 3056), (Cmd::Stack, 0), (Cmd::Deal, 39), (Cmd::Cut, 7011), (Cmd::Deal, 48), (Cmd::Cut, -9660), (Cmd::Deal, 56), (Cmd::Cut, -6843), (Cmd::Stack, 0), (Cmd::Cut, 5111), (Cmd::Deal, 29), (Cmd::Cut, -7700), (Cmd::Stack, 0), (Cmd::Deal, 23), (Cmd::Cut, -5263), (Cmd::Deal, 61), (Cmd::Stack, 0)];
const LEN: usize = 10007;

fn main() {
  let now = Instant::now();
  let input = INPUT.iter()
    .map(|&(cmd,n)| {
      let n = if n < 0 { n + LEN as i64 } else { n };
      (cmd, n as usize)
    })
    .collect::<Vec<_>>();
  let answer = input.iter().fold(
    2019,
    |pos, &(cmd,n)| match cmd {
      Cmd::Deal  => (pos * n) % LEN,
      Cmd::Cut   => (pos - n + LEN) % LEN,
      Cmd::Stack => LEN - 1 - pos,
    }
  );
  println!("Answer: {}", answer);
  println!("Time: {}ms", now.elapsed().as_millis());
}
