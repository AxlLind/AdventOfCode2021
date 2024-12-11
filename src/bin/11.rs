use hashbrown::HashMap;

#[aoc::main(11)]
fn main(input: &str) -> (usize, usize) {
    let mut stones = input.split_whitespace().map(|w| (w.parse::<i64>().unwrap(), 1)).collect::<HashMap<i64, usize>>();
    for _ in 0..75 {
        let mut new_stones = HashMap::with_capacity(stones.len());
        for (&s, &v) in &stones {
            match s {
                0 => *new_stones.entry(1).or_default() += v,
                _ => {
                    let x = s.to_string();
                    if x.len() % 2 == 0 {
                        *new_stones.entry(x[0..x.len() / 2].parse().unwrap()).or_default() += v;
                        *new_stones.entry(x[x.len() / 2..].parse().unwrap()).or_default() += v;
                    } else {
                        *new_stones.entry(s * 2024).or_default() += v
                    }
                }
            }
        }
        stones = new_stones;
    }
    (stones.values().sum(), 0)
}
