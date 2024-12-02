use itertools::Itertools;

fn is_safe(x: &[i64]) -> bool {
    let mut ok = false;
    ok |= x.iter().tuple_windows().all(|(a, b)| a < b);
    ok |= x.iter().tuple_windows().all(|(a, b)| a > b);
    ok && x.iter().tuple_windows().all(|(a, b)| (1..=3).contains(&(a - b).abs()))
}

fn is_any_safe(x: &[i64]) -> bool {
    (0..x.len()).any(|i| {
        let mut y = x.to_vec();
        y.remove(i);
        is_safe(&y)
    })
}

#[aoc::main(02)]
fn main(input: &str) -> (usize, usize) {
    let xs = input.split('\n').map(|l| {
        l.split(' ').map(|w| w.parse().unwrap()).collect::<Vec<_>>()
    }).collect::<Vec<_>>();
    let p1 = xs.iter().filter(|x| is_safe(x)).count();
    let p2 = xs.iter().filter(|x| is_any_safe(x)).count();
    (p1, p2)
}
