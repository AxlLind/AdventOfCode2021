use itertools::Itertools;

fn solve(x1: i64, x2: i64, y1: i64, y2: i64, z1: i64, z2: i64) -> i64 {
    let b = (z2 * x1 - z1 * x2) / (y2 * x1 - y1 * x2);
    let a = (z1 - b * y1) / x1;
    if (x1 * a + y1 * b, x2 * a + y2 * b) != (z1, z2) {
        return 0;
    }
    a * 3 + b
}

#[aoc::main(13)]
fn main(input: &str) -> (i64, i64) {
    let (mut p1, mut p2) = (0, 0);
    for l in input.split("\n\n") {
        let (x1, x2, y1, y2, z1, z2) = l
            .split(|c: char| !c.is_ascii_digit())
            .filter(|w| !w.is_empty())
            .map(|w| w.parse().unwrap())
            .collect_tuple()
            .unwrap();
        p1 += solve(x1, x2, y1, y2, z1, z2);
        p2 += solve(x1, x2, y1, y2, z1 + 10000000000000, z2 + 10000000000000);
    }
    (p1, p2)
}
