use hashbrown::HashMap;

#[aoc::main(03)]
fn main(input: &str) -> (usize, usize) {
  let lines = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  let mut symbols = HashMap::new();
  for (r, l) in lines.iter().enumerate() {
    let mut c = 0;
    while c < l.len() {
      let (start, mut symbol) = (c, None);
      while c < l.len() && l[c].is_ascii_digit() {
        for (dr, dc) in [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)] {
          let (rr, cc) = ((r as i32 + dr) as usize, (c as i32 + dc) as usize);
          let Some(&s) = lines.get(rr).and_then(|l| l.get(cc)) else {continue};
          if s != b'.' && !s.is_ascii_digit() {
            symbol = Some((cc, rr, s as char));
            break;
          }
        }
        c += 1;
      }
      if start < c {
        if let Some(symbol) = symbol {
          let num = l[start..c].iter().fold(0, |n, c| n * 10 + (c - b'0') as usize);
          symbols.entry(symbol).or_insert(Vec::new()).push(num);
        }
      }
      c += 1;
    }
  }
  let p1 = symbols.values().flatten().sum();
  let p2 = symbols.iter()
    .filter(|(&(_,_,s),v)| s == '*' && v.len() == 2)
    .map(|(_,v)| v[0] * v[1])
    .sum();
  (p1, p2)
}
