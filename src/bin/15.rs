use std::collections::VecDeque;
use hashbrown::HashSet;
use itertools::Itertools;

fn solve(mut g: Vec<Vec<u8>>, insts: &str) -> usize {
    let (mut r, mut c) = (0, 0);
    for rr in 0..g.len() {
        for cc in 0..g[0].len() {
            if g[rr][cc] == b'@' {
                g[rr][cc] = b'.';
                (r, c) = (rr, cc);
            }
        }
    }

    'outer: for i in insts.bytes() {
        let (dr, dc) = match i {
            b'^' => (-1,  0),
            b'>' => ( 0,  1),
            b'v' => ( 1,  0),
            b'<' => ( 0, -1),
            _ => continue,
        };
        let (rr, cc) = (r + dr as usize, c + dc as usize);
        let mut q = VecDeque::from([(r, c)]);
        let mut seen = HashSet::new();
        while let Some((rr, cc)) = q.pop_front() {
            if !seen.insert((rr, cc)) {
                continue;
            }
            let (r2, c2) = (rr + dr as usize, cc + dc as usize);
            match g[r2][c2] {
                b'#' => continue 'outer,
                b'O' => {
                    q.push_back((r2, c2));
                }
                b'[' => {
                    q.push_back((r2, c2));
                    q.push_back((r2, c2 + 1));
                }
                b']' => {
                    q.push_back((r2, c2));
                    q.push_back((r2, c2 - 1));
                }
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
        (r, c) = (rr, cc);
    }

    let mut ans = 0;
    for r in 0..g.len() {
        for c in 0..g[0].len() {
            if g[r][c] == b'O' || g[r][c] == b'[' {
                ans += 100 * r + c;
            }
        }
    }
    ans
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
