use std::collections::VecDeque;
use itertools::Itertools;

fn bfs(coords: &[(usize, usize)]) -> Option<usize> {
    let mut q = VecDeque::from([(70, 70, 0)]);
    let mut seen = [[false; 71]; 71];
    let mut g = [[false; 71]; 71];
    for &(r, c) in coords {
        g[r][c] = true;
    }
    while let Some((r, c, n)) = q.pop_front() {
        if (r, c) == (0, 0) {
            return Some(n);
        }
        if seen[r][c] {
            continue;
        }
        seen[r][c] = true;
        for (dr, dc) in [(0, -1), (0, 1), (-1, 0), (1, 0)] {
            let (rr, cc) = (r + dr as usize, c + dc as usize);
            if rr < 71 && cc < 71 && !g[rr][cc] {
                q.push_back((rr, cc, n + 1));
            }
        }
    }
    None
}

#[aoc::main(18)]
fn main(input: &str) -> (usize, String) {
    let coords = input
        .split(|c: char| !c.is_ascii_digit())
        .filter_map(|w| w.parse().ok())
        .tuples()
        .collect::<Vec<_>>();
    let (mut low, mut high) = (1024, coords.len());
    while low <= high {
        let mid = (low + high) / 2;
        if bfs(&coords[..mid]).is_none() {
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    let (r, c) = coords[high];
    (bfs(&coords[..1024]).unwrap(), format!("{r},{c}"))
}
