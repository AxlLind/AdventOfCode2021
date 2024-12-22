use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

#[aoc::main(22)]
fn main(input: &str) -> (i64, i64) {
    let (mut p1, mut p2) = (0, HashMap::new());
    let mut seen = HashSet::new();
    for l in input.lines() {
        let mut ps = [0; 2001];
        let mut p = l.parse().unwrap();
        ps[0] = p;
        for i in 0..2000 {
            p = (p ^ (p * 64))   % 16777216;
            p = (p ^ (p / 32))   % 16777216;
            p = (p ^ (p * 2048)) % 16777216;
            ps[i + 1] = p;
        }
        p1 += p;

        for p in &mut ps {
            *p %= 10;
        }
        seen.clear();
        for (a, b, c, d, e) in ps.iter().tuple_windows() {
            let k = (b - a) + (c - b) * 100 + (d - c) * 10000 + (e - d) * 1000000;
            if seen.insert(k) {
                *p2.entry(k).or_default() += *e;
            }
        }
    }
    (p1, *p2.values().max().unwrap())
}
