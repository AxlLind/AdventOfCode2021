use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

#[aoc::main(08)]
fn main(input: &str) -> (usize, usize) {
    let grid = input.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let mut nodes = HashMap::<_, Vec<_>>::new();
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if grid[i][j] != b'.' {
                nodes.entry(grid[i][j]).or_default().push((i, j));
            }
        }
    }

    let (mut p1, mut p2) = (HashSet::new(), HashSet::new());
    for v in nodes.values() {
        for (&a, &b) in v.iter().tuple_combinations() {
            for ((r1, c1), (r2, c2)) in [(a, b), (b, a)] {
                let (dr, dc) = (r2 - r1, c2 - c1);
                let mut c = c2 + dc;
                let mut r = r2 + dr;

                if (0..grid.len()).contains(&r) && (0..grid[0].len()).contains(&c) {
                    p1.insert((r, c));
                }

                p2.insert((r2, c2));
                while (0..grid.len()).contains(&r) && (0..grid[0].len()).contains(&c) {
                    p2.insert((r, c));
                    c += dc;
                    r += dr;
                }
            }
        }
    }
    (p1.len(), p2.len())
}
