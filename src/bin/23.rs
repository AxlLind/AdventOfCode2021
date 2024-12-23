use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

fn bron_kerbosch<'a>(
    g: &HashMap<&'a str, HashSet<&'a str>>,
    r: &mut HashSet<&'a str>,
    mut p: HashSet<&'a str>,
    mut x: HashSet<&'a str>,
    cliques: &mut Vec<HashSet<&'a str>>,
) {
    if p.is_empty() {
        if x.is_empty() {
            cliques.push(r.clone());
        }
        return;
    }
    for node in p.clone() {
        let neighbours = &g[&node];
        let p2 = p.intersection(neighbours).copied().collect();
        let x2 = x.intersection(neighbours).copied().collect();
        r.insert(node);
        bron_kerbosch(g, r, p2, x2, cliques);
        r.remove(node);
        p.remove(node);
        x.insert(node);
    }
}

#[aoc::main(23)]
fn main(input: &str) -> (usize, String) {
    let mut g = HashMap::<_, HashSet<_>>::new();
    for l in input.lines() {
        let (a, b) = l.split_once('-').unwrap();
        g.entry(a).or_default().insert(b);
        g.entry(b).or_default().insert(a);
    }
    let mut three_cliques = HashSet::new();
    for &n1 in g.keys() {
        if !n1.starts_with('t') {
            continue;
        }
        for &n2 in &g[n1] {
            for &n3 in g[n1].intersection(&g[n2]) {
                let mut c = [n1, n2, n3];
                c.sort();
                three_cliques.insert(c);
            }
        }
    }

    let mut cliques = Vec::new();
    bron_kerbosch(&g, &mut HashSet::new(), g.keys().copied().collect(), HashSet::new(), &mut cliques);
    let p2 = cliques.iter().max_by_key(|c| c.len()).unwrap().iter().sorted().join(",");
    (three_cliques.len(), p2)
}
