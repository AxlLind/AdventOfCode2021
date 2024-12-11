use hashbrown::HashMap;

fn update(old_stones: &HashMap<i64, usize>) -> HashMap<i64, usize> {
    let mut stones = HashMap::with_capacity(old_stones.len());
    for (&s, &v) in old_stones {
        match s {
            0 => *stones.entry(1).or_default() += v,
            s if s.ilog10() % 2 == 1 => {
                let m = 10i64.pow((s.ilog10() + 1) / 2);
                *stones.entry(s % m).or_default() += v;
                *stones.entry(s / m).or_default() += v;
            }
            _ => *stones.entry(s * 2024).or_default() += v,
        }
    }
    stones
}

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
    let mut stones = input.split(' ')
        .map(|w| (w.parse().unwrap(), 1))
        .collect::<HashMap<_, _>>();
    let mut p1 = 0;
    for i in 0..75 {
        if i == 25 {
            p1 = stones.values().sum();
        }
        stones = update(&stones);
    }
    (p1, stones.values().sum())
}
