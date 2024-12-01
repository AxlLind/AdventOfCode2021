use hashbrown::HashMap;

#[aoc::main(01)]
fn main(input: &str) -> (i64, i64) {
    let (mut l1, mut l2): (Vec<_>, Vec<_>) = input.split('\n').map(|l| {
        let (a, b) = l.split_once(' ').unwrap();
        (a.trim().parse::<i64>().unwrap(), b.trim().parse::<i64>().unwrap())
    }).unzip();
    l1.sort();
    l2.sort();

    let mut count = HashMap::new();
    for &b in &l2 {
        *count.entry(b).or_insert(0) += 1;
    }

    let p1 = l1.iter().zip(&l2).map(|(a, b)| (a - b).abs()).sum();
    let p2 = l1.iter().map(|&a| a * count.get(&a).unwrap_or(&0)).sum();
    (p1, p2)
}
