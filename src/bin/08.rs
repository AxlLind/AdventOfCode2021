use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn add_node(grid: &[&[u8]], antinodes: &mut HashSet<(usize, usize)>, (r1, c1): (usize, usize), (r2, c2): (usize, usize), part_one: bool) {
    let (dr, dc) = (r2 - r1, c2 - c1);
    let mut c = c2 + dc;
    let mut r = r2 + dr;
    if part_one {
        if (0..grid.len()).contains(&r) && (0..grid[0].len()).contains(&c) {
            antinodes.insert((r, c));
        }
    } else {
        antinodes.insert((r2, c2));
        while (0..grid.len()).contains(&r) && (0..grid[0].len()).contains(&c) {
            antinodes.insert((r, c));
            c += dc;
            r += dr;
        }
    }
}

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

    let mut p1 = HashSet::new();
    let mut p2 = HashSet::new();
    for v in nodes.values() {
        for (&a, &b) in v.iter().tuple_combinations() {
            add_node(&grid, &mut p1, a, b, true);
            add_node(&grid, &mut p1, b, a, true);
            add_node(&grid, &mut p2, a, b, false);
            add_node(&grid, &mut p2, b, a, false);
        }
    }
    (p1.len(), p2.len())
}
