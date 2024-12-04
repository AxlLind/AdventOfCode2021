fn get(m: &[&[u8]], r: usize, c: usize) -> u8 {
    *m.get(r).and_then(|row| row.get(c)).unwrap_or(&b'_')
}

fn find_xmas(m: &[&[u8]], r: usize, c: usize) -> usize {
    [(0, -1), (-1, 0), (0, 1), (1, 0), (1,1), (-1,1), (1,-1), (-1,-1)].iter()
        .filter(|(dr, dc)|
            (1..4).all(|i| {
                let (rr, cc) = (r + (dr * i) as usize, c + (dc * i) as usize);
                get(m, rr, cc) == b"XMAS"[i as usize]
            })
        )
        .count()
}

fn find_x_mas(m: &[&[u8]], r: usize, c: usize) -> bool {
    let w1 = [get(m, r-1, c-1), get(m, r+1, c+1)];
    let w2 = [get(m, r-1, c+1), get(m, r+1, c-1)];
    (&w1 == b"MS" || &w1 == b"SM") && (&w2 == b"MS" || &w2 == b"SM")
}

#[aoc::main(04)]
fn main(input: &str) -> (usize, usize) {
    let m = input.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let (mut p1, mut p2) = (0, 0);
    for r in 0..m.len() {
        for c in 0..m[0].len() {
            match m[r][c] {
                b'X' => p1 += find_xmas(&m, r, c),
                b'A' => p2 += find_x_mas(&m, r, c) as usize,
                _ => {},
            }
        }
    }
    (p1, p2)
}
