use std::collections::VecDeque;

use hashbrown::HashSet;
use itertools::Itertools;

fn bfs(g: &[[bool; 71]]) -> Option<usize> {
    let mut q = VecDeque::from([(70, 70, 0)]);
    let mut seen = HashSet::new();
    while let Some((r, c, n)) = q.pop_front() {
        if (r, c) == (0, 0) {
            return Some(n);
        }
        if !seen.insert((r, c)) {
            continue;
        }
        for (dr, dc) in [(0, -1), (0, 1), (-1, 0), (1, 0)] {
            let (rr, cc) = (r + dr as usize, c + dc as usize);
            if g.get(rr).and_then(|row| row.get(cc)).copied().unwrap_or(true) {
                continue;
            }
            q.push_back((rr, cc, n + 1));
        }
    }
    None
}

#[aoc::main(18)]
fn main(input: &str) -> (usize, String) {
    let mut coords = input
        .split(|c: char| !c.is_ascii_digit() && c != '-')
        .filter(|w| !w.is_empty())
        .map(|w| w.parse::<usize>().unwrap())
        .tuples();
    let mut g = [[false; 71]; 71];
    for _ in 0..1024 {
        let (r, c) = coords.next().unwrap();
        g[r][c] = true;
    }
    let p1 = bfs(&g).unwrap();
    for (r, c) in coords {
        g[r][c] = true;
        if bfs(&g).is_none() {
            return (p1, format!("{r},{c}"));
        }
    }
    unreachable!()
}
