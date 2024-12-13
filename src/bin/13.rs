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
    let xs = input.split("\n\n").map(|l| {
        let (a,rest) = l.split_once('\n').unwrap();
        let (b, price) = rest.split_once('\n').unwrap();
        let (x1, x2) = a["Button A: X+".len()..].split_once(", Y+").unwrap();
        let (y1, y2) = b["Button B: X+".len()..].split_once(", Y+").unwrap();
        let (z1, z2) = price["Prize: X=".len()..].split_once(", Y=").unwrap();
        (
            x1.parse::<i64>().unwrap(),
            x2.parse::<i64>().unwrap(),
            y1.parse::<i64>().unwrap(),
            y2.parse::<i64>().unwrap(),
            z1.parse::<i64>().unwrap(),
            z2.parse::<i64>().unwrap(),
        )
    });
    let (mut p1, mut p2) = (0, 0);
    for (x1, x2, y1, y2, z1, z2) in xs {
        p1 += solve(x1, x2, y1, y2, z1, z2);
        p2 += solve(x1, x2, y1, y2, z1 + 10000000000000, z2 + 10000000000000);
    }
    (p1, p2)
}
