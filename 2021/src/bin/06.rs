fn simulate_fishes(mut fishes: [usize;9], size: usize) -> usize {
  for _ in 0..size {
    fishes[7] += fishes[0];
    fishes.rotate_left(1);
  }
  fishes.iter().sum()
}

#[aoc::main(06)]
fn main(input: &str) -> (usize,usize) {
  let mut fishes = [0;9];
  for s in input.split(',') {
    let i = s.parse::<usize>().unwrap();
    fishes[i] += 1;
  }
  let p1 = simulate_fishes(fishes, 80);
  let p2 = simulate_fishes(fishes, 256);
  (p1,p2)
}
