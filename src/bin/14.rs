use itertools::Itertools;

fn safety_factor(robots: &[(i64, i64, i64, i64)]) -> usize {
    let mut sectors = [0; 4];
    for &(r, c, _, _) in robots {
        if r == 101 / 2 || c == 103 / 2 {
            continue;
        }
        let a = (r < 101 / 2) as usize;
        let b = (c < 103 / 2) as usize;
        sectors[a * 2 + b] += 1;
    }
    sectors.iter().product()
}

#[aoc::main(14)]
fn main(input: &str) -> (usize, usize) {
    let mut robots = input
        .split(|c: char| !c.is_ascii_digit() && c != '-')
        .filter(|w| !w.is_empty())
        .map(|w| w.parse::<i64>().unwrap())
        .tuples()
        .collect::<Vec<_>>();
    let (mut p1, mut p2) = (0, 0);
    for i in 1.. {
        for (r, c, dr, dc) in &mut robots {
            *r = (*r + *dr).rem_euclid(101);
            *c = (*c + *dc).rem_euclid(103);
        }
        if i == 100 {
            p1 = safety_factor(&robots);
        }
        if robots.iter().map(|&(r, c, _, _)| (r, c)).all_unique() {
            p2 = i;
            break
        }
    }
    (p1, p2)
}
