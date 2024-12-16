use std::{collections::{BinaryHeap, VecDeque}, i64};
use hashbrown::{HashMap, HashSet};

#[aoc::main(16)]
fn main(input: &str) -> (usize, usize) {
    let g = input.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
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

    let mut p1 = i64::MAX;
    let mut seen = HashMap::new();
    let mut q = BinaryHeap::from([(0, 0, start)]);
    while let Some((score, d, (r, c))) = q.pop() {
        let score = -score;
        if (r, c) == end {
            if score > p1 {
                break;
            }
            p1 = score;
        }
        for dd in 0..4 {
            let (dr, dc) = [(1, 0), (0, 1), (-1, 0), (0, -1)][dd];
            let (rr, cc) = (r + dr as usize, c + dc as usize);
            if g[rr][cc] == b'#' {
                continue;
            }

            let s = score + if d == dd {1} else {1001};
            let last_seen = seen.get(&(rr, cc, dd)).copied().unwrap_or(i64::MAX);
            if s <= last_seen {
                seen.insert((rr, cc, dd), s);
                q.push((-s, dd, (rr, cc)));
            }
        }
    }

    let mut p2 = HashSet::new();
    let mut q = VecDeque::new();
    for d in 0..4 {
        if seen.get(&(end.0, end.1, d)).copied().unwrap_or(i64::MAX) == p1 {
            q.push_back((end, d, p1));
        }
    }
    while let Some(((r, c), d, s)) = q.pop_front() {
        p2.insert((r, c));
        for dd in 0..4 {
            let ss = s - if d == dd {1} else {1001};
            let (dr, dc) = [(1, 0), (0, 1), (-1, 0), (0, -1)][d];
            let (rr, cc) = (r - dr as usize, c - dc as usize);
            if seen.get(&(rr, cc, dd)).copied().unwrap_or(i64::MAX) == ss {
                q.push_back(((rr, cc), dd, ss));
            }
        }
    }
    (p1 as _, p2.len() + 1)
}
