use std::collections::VecDeque;
use hashbrown::HashMap;
use itertools::Itertools;

fn count(dists: &HashMap<(usize, usize), usize>, max_step: usize) -> usize {
    let mut ans = 0;
    for ((&(r1, c1), &n1), (&(r2, c2), &n2)) in dists.iter().tuple_combinations() {
        let d = r1.abs_diff(r2) + c1.abs_diff(c2);
        if d > max_step {
            continue;
        }
        let saved = n2.abs_diff(n1) - d;
        if saved >= 100 {
            ans += 1;
        }
    }
    ans
}

#[aoc::main(20)]
fn main(input: &str) -> (usize, usize) {
    let g = input.lines().map(str::as_bytes).collect::<Vec<_>>();
    let (mut start, mut end) = ((0, 0), (0, 0));
    for r in 0..g.len() {
        for c in 0..g[0].len() {
            match g[r][c] {
                b'S' => start = (r, c),
                b'E' => end = (r, c),
                _ => {}
            }
        }
    }
    let mut q = VecDeque::from([(start.0, start.1, 0)]);
    let mut dists = HashMap::new();
    while let Some((r, c, n)) = q.pop_front() {
        if dists.contains_key(&(r, c)) {
            continue;
        }
        dists.insert((r, c), n);
        if (r, c) == end {
            continue;
        }
        for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let (rr, cc) = (r + dr as usize, c + dc as usize);
            let x = g[rr][cc];
            if x != b'#' {
                q.push_back((rr, cc, n+1));
            }
        }
    }
    (count(&dists, 2), count(&dists, 20))
}
