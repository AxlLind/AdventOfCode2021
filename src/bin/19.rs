use hashbrown::HashMap;

fn valid<'a>(cache: &mut HashMap<&'a [u8], usize>, s: &'a [u8], towels: &[&'a [u8]]) -> usize {
    if s.is_empty() {
        return 1;
    }
    if let Some(&ans) = cache.get(&s) {
        return ans;
    }
    let ans = towels.iter()
        .filter(|t| s.starts_with(t))
        .map(|t| valid(cache, &s[t.len()..], towels))
        .sum();
    cache.insert(s, ans);
    ans
}

#[aoc::main(19)]
fn main(input: &str) -> (usize, usize) {
    let (a, b) = input.split_once("\n\n").unwrap();
    let towels = a.split(", ").map(str::as_bytes).collect::<Vec<_>>();
    let mut cache = HashMap::new();
    let (mut p1, mut p2) = (0, 0);
    for p in b.lines().map(str::as_bytes) {
        let v = valid(&mut cache, p, &towels);
        if v > 0 {
            p1 += 1;
            p2 += v;
        }
    }
    (p1, p2)
}
