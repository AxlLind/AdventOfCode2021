use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn prices(mut p: i64) -> Vec<i64> {
    let mut ps = vec![p];
    for _ in 0..2000 {
        p = (p ^ (p * 64))   % 16777216;
        p = (p ^ (p / 32))   % 16777216;
        p = (p ^ (p * 2048)) % 16777216;
        ps.push(p);
    }
    ps
}

#[aoc::main(22)]
fn main(input: &str) -> (i64, i64) {
    let (mut p1, mut p2) = (0, HashMap::new());
    for l in input.lines() {
        let mut ps = prices(l.parse().unwrap());
        p1 += ps.last().unwrap();
        for p in &mut ps {
            *p %= 10;
        }
        let mut seen = HashSet::new();
        for (a, b, c, d, e) in ps.iter().tuple_windows() {
            let k = (b - a, c - b, d - c, e - d);
            if seen.insert(k) {
                *p2.entry(k).or_default() += *e;
            }
        }
    }
    (p1, *p2.values().max().unwrap())
}
