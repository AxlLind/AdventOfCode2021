fn solve(mut files: Vec<(usize, i32)>) -> usize {
    let mut i = files.len() - 1;
    while i > 0 {
        let (size, id) = files[i];
        if id == -1 {
            i -= 1;
            continue;
        }
        if let Some(j) = files[0..i].iter().position(|&(s, id)| id == -1 && size <= s) {
            let s = files[j].0;
            files[j] = (size, id);
            files[i] = (size, -1);
            if size < s {
                files.insert(j+1, (s - size, -1));
            }
        }
        i -= 1;
    }
    files.iter().flat_map(|&(s, id)| (0..s).map(move |_| id)).enumerate().map(|(i, id)| if id == -1 {0} else {i * id as usize}).sum()
}

#[aoc::main(09)]
fn main(input: &str) -> (usize, usize) {
    let mut fs1 = Vec::new();
    let mut fs2 = Vec::new();
    let mut fid = 0;
    for (i, b) in input.bytes().enumerate() {
        let v = if i % 2 == 0 {fid += 1; fid - 1} else {-1};
        fs1.extend((0..b - b'0').map(|_| (1, v)));
        fs2.push(((b - b'0') as usize, v));
    }
    let p1 = solve(fs1);
    let p2 = solve(fs2);
    (p1, p2)
}
