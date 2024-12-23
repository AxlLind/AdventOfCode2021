static INPUT: [usize; 9] = [4,6,3,5,2,8,1,7,9];

fn build_list(size: usize) -> Vec<usize> {
  let mut v = vec![0; size+1];
  let last = (1..size).fold(INPUT[0], |curr, i| {
    v[curr] = if i < 9 {INPUT[i]} else {i+1};
    v[curr]
  });
  v[last] = INPUT[0];
  v
}

fn simulate_game(size: usize, rounds: usize) -> Vec<usize> {
  let mut list = build_list(size);
  let mut curr = INPUT[0];
  for _ in 0..rounds {
    let (a,b,c) = (list[curr], list[list[curr]], list[list[list[curr]]]);
    let mut t = if curr == 1 {size} else {curr-1};
    while t == a || t == b || t == c {
      t = if t == 1 {size} else {t-1};
    }
    list[curr] = list[c];
    list[c] = list[t];
    list[t] = a;
    curr = list[curr];
  }
  list
}

fn part_one() -> usize {
  let list = simulate_game(9, 100);
  let (mut ans, mut curr) = (0, list[1]);
  while curr != 1 {
    ans = ans * 10 + curr;
    curr = list[curr];
  }
  ans
}

fn part_two() -> usize {
  let list = simulate_game(1000000, 10000000);
  list[1] * list[list[1]]
}

aoc2020::main! {
  (part_one(), part_two())
}
