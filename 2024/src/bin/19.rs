use hashbrown::HashMap;

fn ways<'a>(s: &'a [u8], towels: &[&[u8]], cache: &mut HashMap<&'a [u8], usize>) -> usize {
    if s.is_empty() {
        return 1;
    }
    if let Some(&n) = cache.get(&s) {
        return n;
    }
    let n = towels.iter()
        .filter(|t| s.starts_with(t))
        .map(|t| ways(&s[t.len()..], towels, cache))
        .sum();
    cache.insert(s, n);
    n
}

#[aoc::main(19)]
fn main(input: &str) -> (usize, usize) {
    let (a, patterns) = input.split_once("\n\n").unwrap();
    let towels = a.split(", ").map(str::as_bytes).collect::<Vec<_>>();
    let mut cache = HashMap::new();
    let (mut p1, mut p2) = (0, 0);
    for p in patterns.lines() {
        let n = ways(p.as_bytes(), &towels, &mut cache);
        if n > 0 {
            p1 += 1;
            p2 += n;
        }
    }
    (p1, p2)
}
