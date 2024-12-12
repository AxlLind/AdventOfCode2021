use std::collections::VecDeque;
use hashbrown::HashSet;

macro_rules! neighbours {
    ($g:ident, $r:expr, $c:expr) => {
        [($r+1, $c), ($r-1, $c), ($r, $c+1), ($r, $c-1)].iter()
            .copied()
            .filter(|&(rr, cc)| *$g.get(rr).and_then(|row| row.get(cc)).unwrap_or(&0) == $g[$r][$c])
    };
}

fn perimiter(g: &[&[u8]], a: &HashSet<(usize, usize)>) -> usize {
    a.iter().map(|&(r, c)| 4 - neighbours!(g, r, c).count()).sum()
}

fn sides(a: &HashSet<(usize, usize)>) -> usize {
    let mut s = HashSet::new();
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
            s.insert((rr, cc, dr, dc));
        }
    }
    s.len()
}

fn find_shape(g: &[&[u8]], r: usize, c: usize, seen: &mut HashSet<(usize, usize)>) -> HashSet<(usize, usize)> {
    let mut q = VecDeque::from([(r,c)]);
    let mut a = HashSet::from([(r,c)]);
    while let Some((r, c)) = q.pop_front() {
        for (rr, cc) in neighbours!(g, r, c) {
            if seen.insert((rr, cc)) {
                a.insert((rr, cc));
                q.push_back((rr, cc));
            }
        }
    }
    a
}

#[aoc::main(12)]
fn main(input: &str) -> (usize, usize) {
    let g = input.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let mut seen = HashSet::new();
    let (mut p1, mut p2) = (0, 0);
    for r in 0..g.len() {
        for c in 0..g[0].len() {
            if seen.contains(&(r, c)) {
                continue;
            }
            let a = find_shape(&g, r, c, &mut seen);
            p1 += a.len() * perimiter(&g, &a);
            p2 += a.len() * sides(&a);
        }
    }
    (p1, p2)
}
