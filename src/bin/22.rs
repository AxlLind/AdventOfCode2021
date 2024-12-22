use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

#[aoc::main(22)]
fn main(input: &str) -> (i64, i64) {
    let (mut p1, mut p2) = (0, HashMap::new());
    let mut seen = HashSet::new();
    for l in input.lines() {
        let mut ps = [0; 2000];
        let mut p = l.parse::<i64>().unwrap();
        for i in 0..2000 {
            p = (p ^ (p * 64))   % 16777216;
            p = (p ^ (p / 32))   % 16777216;
            p = (p ^ (p * 2048)) % 16777216;
            ps[i] = p % 10;
        }
        p1 += p;

        seen.clear();
        for (a, b, c, d, e) in ps.iter().tuple_windows() {
            let k = (b - a) + (c - b) * 20 + (d - c) * 400 + (e - d) * 8000;
            if seen.insert(k) {
                *p2.entry(k).or_default() += *e;
            }
        }
    }
    (p1, *p2.values().max().unwrap())
}
