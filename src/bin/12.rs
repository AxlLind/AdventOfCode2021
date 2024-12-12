use std::collections::VecDeque;

use hashbrown::HashSet;

fn is_neighbour(g: &[&[u8]], chr: u8, r: usize, c: usize) -> bool {
    *g.get(r).and_then(|row| row.get(c)).unwrap_or(&0) == chr
}

fn sides(a: &HashSet<(usize, usize)>) -> usize {
    let mut seen = HashSet::new();
    for &(r, c) in a {
        for (dr, dc) in [(-1, 0), (0, 1), (1, 0), (0, -1)] {
            if a.contains(&(r + dr as usize, c + dc as usize)) {
                continue;
            }
            let (mut rr, mut cc) = (r, c);
            while a.contains(&(rr + dc as usize, cc + dr as usize)) && !a.contains(&(rr + dr as usize, cc + dc as usize)) {
                rr += dc as usize;
                cc += dr as usize;
            }
            seen.insert((rr, cc, dr, dc));
        }
    }
    seen.len()
}

fn flood(g: &[&[u8]], r: usize, c: usize, seen: &mut HashSet<(usize, usize)>) -> (usize, usize) {
    let mut q = VecDeque::from([(r,c)]);
    let mut a = HashSet::new();
    a.insert((r, c));
    while let Some((r, c)) = q.pop_front() {
        for (rr, cc) in [(r+1, c), (r-1, c), (r, c+1), (r, c-1)] {
            if is_neighbour(g, g[r][c], rr, cc) {
                if seen.insert((rr, cc)) {
                    a.insert((rr, cc));
                    q.push_back((rr, cc));
                }
            }
        }
    }
    let mut perimiter = 0;
    for &(r, c) in &a {
        let ns = [(r+1, c), (r-1, c), (r, c+1), (r, c-1)];
        perimiter += ns.iter().filter(|&&(rr, cc)| !is_neighbour(g, g[r][c], rr, cc)).count();
    }
    (
        a.len() * perimiter,
        a.len() * sides(&a),
    )
}

#[aoc::main(12)]
fn main(input: &str) -> (usize, usize) {
    let grid = input.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let mut seen = HashSet::new();
    let (mut p1, mut p2) = (0, 0);
    for r in 0..grid.len() {
        for c in 0..grid[0].len() {
            if seen.contains(&(r, c)) {
                continue;
            }
            let (a, b) = flood(&grid, r, c, &mut seen);
            p1 += a;
            p2 += b;
        }
    }
    (p1, p2)
}
