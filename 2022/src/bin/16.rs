use hashbrown::HashMap;
use itertools::Itertools;

#[aoc::main(16)]
fn main(input: &str) -> (u16, u16) {
  let valves = input.lines()
    .map(|l| {
      let mut words = l.split(|c: char| !c.is_uppercase() && !c.is_ascii_digit()).filter(|w| !w.is_empty()).skip(1);
      let valve = words.next().unwrap();
      let flow = words.next().unwrap().parse::<u16>().unwrap();
      let tunnels = words.collect::<Vec<_>>();
      (valve, flow, tunnels)
    })
    .sorted_by_key(|(_, flow, _)| -(*flow as i32))
    .collect::<Vec<_>>();
  let labels = valves.iter().enumerate().map(|(i, v)| (v.0, i)).collect::<HashMap<_, _>>();
  let flow = valves.iter().map(|(_,flow,_)| *flow).collect::<Vec<_>>();
  let adj = valves.iter().map(|(_, _, tunnels)| tunnels.iter().map(|t| labels[t]).collect::<Vec<_>>()).collect::<Vec<_>>();
  let m = valves.iter().position(|(_, flow, _)| *flow == 0).unwrap();
  let mm = 1 << m;

  // opt[time][node][available valves]
  let mut opt = vec![vec![vec![0; mm]; valves.len()]; 30];
  for t in 1..30 {
    for i in 0..valves.len() {
      let ii = 1 << i;
      for x in 0..mm {
        let mut tmp = opt[t][i][x];
        if ii & x != 0 && t >= 2 {
          tmp = tmp.max(opt[t-1][i][x - ii] + flow[i] * t as u16);
        }
        opt[t][i][x] = tmp.max(adj[i].iter().map(|&j| opt[t-1][j][x]).max().unwrap());
      }
    }
  }

  let start = labels["AA"];
  let p1 = opt[29][start][mm - 1];
  let p2 = (0..mm/2).map(|x| opt[25][start][x] + opt[25][start][mm-1-x]).max().unwrap();
  (p1, p2)
}
