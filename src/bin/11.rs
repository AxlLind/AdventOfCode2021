use hashbrown::HashMap;

fn update(old_stones: &HashMap<i64, usize>) -> HashMap<i64, usize> {
    let mut stones = HashMap::with_capacity(old_stones.len());
    for (&s, &v) in old_stones {
        match s {
            0 => *stones.entry(1).or_default() += v,
            _ => {
                let x = s.to_string();
                if x.len() % 2 == 0 {
                    *stones.entry(x[0..x.len() / 2].parse().unwrap()).or_default() += v;
                    *stones.entry(x[x.len() / 2..].parse().unwrap()).or_default() += v;
                } else {
                    *stones.entry(s * 2024).or_default() += v
                }
            }
        }
    }
    stones
}

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
    let mut stones = input.split_whitespace().map(|w| (w.parse::<i64>().unwrap(), 1)).collect::<HashMap<i64, usize>>();
    let mut p1 = 0;
    for i in 0..75 {
        stones = update(&stones);
        if i == 25 {
            p1 = stones.values().sum();
        }
    }
    (p1, stones.values().sum())
}
