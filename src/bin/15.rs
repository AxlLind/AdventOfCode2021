use std::collections::VecDeque;
use hashbrown::HashSet;
use itertools::Itertools;

fn solve(mut g: Vec<Vec<u8>>, insts: &str) -> usize {
    let (mut r, mut c) = (0..g.len()).cartesian_product(0..g[0].len())
        .find(|&(r, c)| g[r][c] == b'@')
        .unwrap();
    'outer: for i in insts.bytes() {
        let (dr, dc) = match i {
            b'^' => (-1,  0),
            b'>' => ( 0,  1),
            b'v' => ( 1,  0),
            b'<' => ( 0, -1),
            _ => continue,
        };
        let mut q = VecDeque::from([(r, c)]);
        let mut seen = HashSet::new();
        while let Some((rr, cc)) = q.pop_front() {
            if !seen.insert((rr, cc)) {
                continue;
            }
            let (r2, c2) = (rr + dr as usize, cc + dc as usize);
            match g[r2][c2] {
                b'#' => continue 'outer,
                b'O' => q.push_back((r2, c2)),
                b'[' => q.extend([(r2, c2), (r2, c2 + 1)]),
                b']' => q.extend([(r2, c2), (r2, c2 - 1)]),
                _ => continue,
            }
        }
        while !seen.is_empty() {
            for (rr, cc) in seen.iter().copied().sorted() {
                let (r2, c2) = (rr + dr as usize, cc + dc as usize);
                if !seen.contains(&(r2, c2)) {
                    g[r2][c2] = g[rr][cc];
                    g[rr][cc] = b'.';
                    seen.remove(&(rr, cc));
                }
            }
        }
        (r, c) = (r + dr as usize, c + dc as usize);
    }
    (0..g.len()).cartesian_product(0..g[0].len())
        .filter(|&(r, c)| matches!(g[r][c], b'O' | b'['))
        .map(|(r, c)| r * 100 + c)
        .sum()
}

#[aoc::main(15)]
fn main(input: &str) -> (usize, usize) {
    let (a, insts) = input.split_once("\n\n").unwrap();
    let g1 = a.lines().map(|l| l.as_bytes().to_vec()).collect();
    let g2 = a.lines().map(|l| l.bytes().flat_map(|b| match b {
        b'#' => b"##",
        b'O' => b"[]",
        b'.' => b"..",
        b'@' => b"@.",
        _ => unreachable!(),
    }).copied().collect()).collect();
    (solve(g1, insts), solve(g2, insts))
}
