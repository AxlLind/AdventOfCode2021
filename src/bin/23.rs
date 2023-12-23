use hashbrown::HashMap;
use itertools::Itertools;

const NEIGHBORS: &[(isize,isize)] = &[(-1,0),(0,1),(1,0),(0,-1)];

#[allow(clippy::type_complexity)]
fn dfs(
  graph: &HashMap<(usize,usize), Vec<(usize,usize,usize)>>,
  seen: &mut Vec<Vec<bool>>,
  (r,c): (usize,usize),
) -> Option<usize> {
  if r == seen.len() - 1 {
    return Some(0);
  }
  let mut max_dist = None;
  for &(rr, cc, d) in &graph[&(r,c)] {
    if !seen[rr][cc] {
      seen[rr][cc] = true;
      if let Some(dist) = dfs(graph, seen, (rr, cc)) {
        max_dist = Some(max_dist.unwrap_or(0).max(d+dist))
      }
      seen[rr][cc] = false;
    }
  }
  max_dist
}

fn solve(grid: &[&[u8]], part2: bool) -> usize {
  let mut graph = HashMap::<_,Vec<_>>::new();
  for (r,c) in (0..grid.len()).cartesian_product(0..grid[0].len()) {
    let neighbors = match grid[r][c] {
      b'#' => continue,
      _ if part2 => NEIGHBORS,
      b'.' => NEIGHBORS,
      b'^' => &NEIGHBORS[0..][..1],
      b'>' => &NEIGHBORS[1..][..1],
      b'v' => &NEIGHBORS[2..][..1],
      b'<' => &NEIGHBORS[3..][..1],
      _ => unreachable!(),
    };
    let e = graph.entry((r,c)).or_default();
    for (dr, dc) in neighbors {
      let rr = (r as isize + dr) as usize;
      let cc = (c as isize + dc) as usize;
      let Some(&tile) = grid.get(rr).and_then(|row| row.get(cc)) else {continue};
      if tile != b'#' {
        e.push((rr,cc,1));
      }
    }
  }
  while let Some((&(r,c), _)) = graph.iter().find(|(_,n)| n.len() == 2) {
    let neighbors = graph.remove(&(r,c)).unwrap();
    let (r1,c1,d1) = neighbors[0];
    let (r2,c2,d2) = neighbors[1];
    let n1 = graph.get_mut(&(r1,c1)).unwrap();
    if let Some(i) = n1.iter().position(|&(rr,cc,_)| (rr,cc) == (r,c)) {
      n1[i] = (r2,c2,d1+d2);
    }
    let n2 = graph.get_mut(&(r2,c2)).unwrap();
    if let Some(i) = n2.iter().position(|&(rr,cc,_)| (rr,cc) == (r,c)) {
      n2[i] = (r1,c1,d1+d2);
    }
  }
  dfs(&graph, &mut vec![vec![false; grid[0].len()]; grid.len()], (0,1)).unwrap()
}

#[aoc::main(23)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  (solve(&grid, false), solve(&grid, true))
}
