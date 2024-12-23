use hashbrown::HashMap;

fn find_symbol(lines: &[&[u8]], r: usize, c: usize) -> Option<(usize, usize, char)> {
  for (dr, dc) in [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)] {
    let rr = (r as i32 + dr) as usize;
    let cc = (c as i32 + dc) as usize;
    let Some(&s) = lines.get(rr).and_then(|l| l.get(cc)) else { continue };
    if s != b'.' && !s.is_ascii_digit() {
      return Some((cc, rr, s as char));
    }
  }
  None
}

#[aoc::main(03)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let mut symbols = HashMap::<_,Vec<_>>::new();
  for (r, l) in lines.iter().enumerate() {
    let mut c = 0;
    while c < l.len() {
      let (start, mut symbol) = (c, None);
      while c < l.len() && l[c].is_ascii_digit() {
        if symbol.is_none() {
          symbol = find_symbol(&lines, r, c);
        }
        c += 1;
      }
      if let Some(symbol) = symbol {
        let num = l[start..c].iter().fold(0, |n, c| n * 10 + (c - b'0') as usize);
        symbols.entry(symbol).or_default().push(num);
      }
      c += 1;
    }
  }
  let p2 = symbols.iter()
    .filter(|(&(_,_,s),v)| s == '*' && v.len() == 2)
    .map(|(_,v)| v[0] * v[1])
    .sum();
  (symbols.values().flatten().sum(), p2)
}
