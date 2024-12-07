fn is_valid(target: i64, ns: &[i64], n: i64, p2: bool) -> bool {
    if ns.is_empty() || n > target {
        return n == target;
    }
    (p2 && is_valid(target, &ns[1..], n * 10i64.pow(ns[0].ilog10() + 1) + ns[0], p2))
        || is_valid(target, &ns[1..], n + ns[0], p2)
        || is_valid(target, &ns[1..], n * ns[0], p2)
}

#[aoc::main(07)]
fn main(input: &str) -> (i64, i64) {
    let ops = input.lines().map(|l| {
        let (n, rest) = l.split_once(": ").unwrap();
        let ns = rest.split(' ').map(|w| w.parse().unwrap()).collect::<Vec<_>>();
        (n.parse::<i64>().unwrap(), ns)
    });

    let (mut p1, mut p2) = (0, 0);
    for (n, ns) in ops {
        if is_valid(n, &ns[1..], ns[0], false) {
            p1 += n;
        } else if is_valid(n, &ns[1..], ns[0], true) {
            p2 += n;
        }
    }
    (p1, p1 + p2)
}
