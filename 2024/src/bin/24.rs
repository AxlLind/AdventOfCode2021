use hashbrown::HashMap;
use itertools::Itertools;

fn val<'a>(g: &HashMap<&'a str, (&'a str, &'a str, &'a str)>, s: &mut HashMap<&'a str, bool>, n: &'a str) {
    if s.contains_key(n) {
        return;
    }
    let (a, op, b) = g[n];
    val(g, s, a);
    val(g, s, b);
    let v = match op {
        "AND" => s[a] && s[b],
        "XOR" => s[a] != s[b],
        "OR"  => s[a] || s[b],
        _ => unreachable!(),
    };
    s.insert(n, v);
}

fn v(s: &HashMap<&str, bool>, p: char) -> usize {
    let mut v = 0;
    for &n in s.keys() {
        if !n.starts_with(p) {
            continue;
        }
        let i = n[1..].parse::<usize>().unwrap();
        if s[n] {
            v |= 1 << i;
        }
    }
    v
}

#[aoc::main(24)]
fn main(input: &str) -> (usize, &'static str) {
    let (s1, s2) = input.split_once("\n\n").unwrap();
    let mut s_start = HashMap::new();
    for l in s1.lines() {
        let (a, x) = l.split_once(": ").unwrap();
        s_start.insert(a, x == "1");
    }
    let mut s = s_start.clone();
    let mut g = HashMap::new();
    for l in s2.lines() {
        let (a, op, b, _, c) = l.split(' ').collect_tuple().unwrap();
        g.insert(c, (a, op, b));
    }
    for n in g.keys() {
        val(&g, &mut s, n);
    }
    // Solved by hand
    (v(&s, 'z'), "jqf,mdd,skh,wpd,wts,z11,z19,z37")
}
