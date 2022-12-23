use hashbrown::{HashMap, HashSet};
use itertools::Itertools;

#[aoc::main(23)]
fn main(input: &str) -> (usize, usize) {
  let mut state = input.lines()
    .enumerate()
    .flat_map(|(x,l)| l.bytes()
      .enumerate()
      .filter(|&(_,b)| b == b'#')
      .map(move |(y,_)| (x as i32, y as i32))
    )
    .collect::<HashSet<_>>();
  let (mut p1, mut p2) = (0,0);
  for t in 0.. {
    let mut proposals = HashMap::<_,Vec<_>>::new();
    for &(x,y) in &state {
      let ns = [
        state.contains(&(x-1,y-1)), state.contains(&(x-1,y)), state.contains(&(x-1,y+1)),
        state.contains(&(x,  y-1)),                           state.contains(&(x,  y+1)),
        state.contains(&(x+1,y-1)), state.contains(&(x+1,y)), state.contains(&(x+1,y+1)),
      ];
      if ns.iter().all(|b| !b) {
        continue;
      }
      let props = [
        (!ns[0] && !ns[1] && !ns[2], (x-1,y)),
        (!ns[5] && !ns[6] && !ns[7], (x+1,y)),
        (!ns[0] && !ns[3] && !ns[5], (x,y-1)),
        (!ns[2] && !ns[4] && !ns[7], (x,y+1)),
      ];
      for i in 0..4 {
        let (free, pos) = props[(t + i) % 4];
        if free {
          proposals.entry(pos).or_default().push((x,y));
          break;
        }
      }
    }
    let mut not_done = false;
    for (pos, props) in proposals {
      if props.len() == 1 {
        not_done = true;
        state.remove(&props[0]);
        state.insert(pos);
      }
    }
    if !not_done {
      p2 = t+1;
      break;
    }
    if t == 9 {
      let (&minx, &maxx) = state.iter().map(|(x,_)| x).minmax().into_option().unwrap();
      let (&miny, &maxy) = state.iter().map(|(_,y)| y).minmax().into_option().unwrap();
      p1 = (minx..=maxx).cartesian_product(miny..=maxy).filter(|p| !state.contains(p)).count();
    }
  }
  (p1,p2)
}
