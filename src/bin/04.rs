#[aoc::main(04)]
fn main(input: &str) -> (usize, usize) {
  let pairs = input.lines().filter_map(|l| {
    let (x,y) = l.split_once(',')?;
    let (a,b) = x.split_once('-')?;
    let (c,d) = y.split_once('-')?;
    Some((a.parse().ok()?, b.parse().ok()?, c.parse().ok()?, d.parse().ok()?))
  }).collect::<Vec<(usize,_,_,_)>>();
  let p1 = pairs.iter()
    .filter(|(x1,y1,x2,y2)| (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1))
    .count();
  let p2 = pairs.iter()
    .filter(|(x1,y1,x2,y2)| y1 >= x2 && x1 <= y2)
    .count();
  (p1, p2)
}
