use itertools::Itertools;

fn find_start(m: &[Vec<u8>]) -> (usize, usize) {
    for r in 0..m.len() {
        for c in 0..m[0].len() {
            if m[r][c] == b'^' {
                return (r, c);
            }
        }
    }
    unreachable!()
}

fn walk(m: &[Vec<u8>], mut r: usize, mut c: usize) -> Option<Vec<(usize, usize)>> {
    let mut seen = vec![vec![[false; 4]; m[0].len()]; m.len()];
    let mut d = 0;
    loop {
        if seen[r][c][d] {
            return None;
        }
        seen[r][c][d] = true;
        let (dr, dc) = [(-1,0), (0,1), (1,0), (0, -1)][d];
        let (rr, cc) = (r + dr as usize, c + dc as usize);
        if !(0..m.len()).contains(&rr) || !(0..m[0].len()).contains(&cc) {
            let visited = (0..m.len()).cartesian_product(0..m[0].len())
                .filter(|&(r, c)| seen[r][c].iter().any(|&b| b))
                .collect();
            return Some(visited);
        }
        if m[rr][cc] == b'#' {
            d = (d + 1) % 4;
        } else {
            (r, c) = (rr, cc);
        }
    }
}

#[aoc::main(06)]
fn main(input: &str) -> (usize, usize) {
    let mut m = input.lines().map(|l| l.as_bytes().to_vec()).collect::<Vec<_>>();
    let (sr, sc) = find_start(&m);
    let p1 = walk(&m, sr, sc).unwrap();
    let p2 = p1.iter().filter(|&&(r, c)| {
        let saved = m[r][c];
        m[r][c] = b'#';
        let ok = walk(&m, sr, sc).is_none();
        m[r][c] = saved;
        ok
    }).count();
    (p1.len(), p2)
}
