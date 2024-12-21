use std::collections::BinaryHeap;
use hashbrown::HashMap;

const PAD1: &[&[u8]] = &[b"789", b"456", b"123", b" 0A"];
const PAD2: &[&[u8]] = &[b" ^A", b"<v>"];

fn pad_move(r: usize, c: usize, m: u8, pad: &[&[u8]]) -> (usize, usize, Option<u8>) {
    match m {
        b'<' => (r, c - 1, None),
        b'^' => (r - 1, c, None),
        b'>' => (r, c + 1, None),
        b'v' => (r + 1, c, None),
        b'A' => (r, c, Some(pad[r][c])),
        _ => unreachable!(),
    }
}

fn calculate_cost(cache: &mut HashMap<(u8, u8, usize), usize>, goal: u8, prev_m: u8, pads: usize) -> usize {
    if pads == 0 {
        return 1;
    }
    if let Some(&d) = cache.get(&(goal, prev_m, pads)) {
        return d;
    }
    let start = match prev_m {
        b'^' => (0, 1),
        b'A' => (0, 2),
        b'<' => (1, 0),
        b'v' => (1, 1),
        b'>' => (1, 2),
        _ => unreachable!(),
    };
    let mut q = BinaryHeap::from([(0, start, b'A', 0)]);
    while let Some((d, (r, c), prev, out)) = q.pop() {
        let d = (-d) as usize;
        if out == goal {
            cache.insert((goal, prev_m, pads), d);
            return d;
        }
        for &m in b"A^<v>" {
            let (rr, cc, x) = pad_move(r, c, m, PAD2);
            if *PAD2.get(rr).and_then(|row| row.get(cc)).unwrap_or(&b' ') == b' ' {
                continue;
            }
            let x = x.unwrap_or(0);
            if x != 0 && x != goal {
                continue;
            }
            let d = d + calculate_cost(cache, m, prev, pads - 1);
            q.push((-(d as i64), (rr, cc), m, x));
        }
    }
    unreachable!()
}

fn solve(cache: &mut HashMap<(u8, u8, usize), usize>, code: &[u8], pads: usize) -> usize {
    let mut q = BinaryHeap::from([(0, (3, 2), b'A', 0)]);
    let mut seen = HashMap::new();
    while let Some((d, (r, c), prev, l)) = q.pop() {
        let d = (-d) as usize;
        if l == code.len() {
            return d;
        }
        let k = ((r, c), prev, l);
        if seen.contains_key(&k) {
            continue;
        }
        seen.insert(k, d);
        for &m in b"A^<v>" {
            let (rr, cc, x) = pad_move(r, c, m, PAD1);
            if *PAD1.get(rr).and_then(|row| row.get(cc)).unwrap_or(&b' ') == b' ' {
                continue;
            }
            let mut l = l;
            if let Some(x) = x {
                if x != code[l] {
                    continue;
                }
                l += 1;
            }
            let d = d + calculate_cost(cache, m, prev, pads);
            q.push((-(d as i64), (rr, cc), m, l));
        }
    }
    unreachable!()
}

#[aoc::main(21)]
fn main(input: &str) -> (usize, usize) {
    let (mut p1, mut p2) = (0, 0);
    let mut cache = HashMap::new();
    for l in input.lines() {
        let n = l.strip_suffix('A').unwrap().parse::<usize>().unwrap();
        p1 += n * solve(&mut cache, l.as_bytes(), 2);
        p2 += n * solve(&mut cache, l.as_bytes(), 25);
    }
    (p1, p2)
}
