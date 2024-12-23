use hashbrown::HashMap;
use itertools::Itertools;

fn dfs(graph: &[Vec<(usize,usize)>], seen: &mut [bool], goal: usize, curr: usize) -> Option<usize> {
  if curr == goal {
    return Some(0);
  }
  let mut max_dist = None;
  for &(next, d) in &graph[curr] {
    if !seen[next] {
      seen[next] = true;
      if let Some(dist) = dfs(graph, seen, goal, next) {
        max_dist = Some(max_dist.unwrap_or(0).max(d+dist))
      }
      seen[next] = false;
    }
  }
  max_dist
}

fn solve(grid: &[&[u8]], part2: bool) -> usize {
  let mut graph = HashMap::<_,Vec<_>>::new();
  for (r,c) in (0..grid.len()).cartesian_product(0..grid[0].len()) {
    const NEIGHBORS: &[(isize,isize)] = &[(-1,0),(0,1),(1,0),(0,-1)];
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
      if grid.get(rr).and_then(|row| row.get(cc)).is_some_and(|&t| t != b'#') {
        e.push((rr,cc,1));
      }
    }
  }

  // edge contraction (i.e contract maze corridors)
  let corridors = graph.iter()
    .filter(|(_,n)| n.len() == 2)
    .map(|(&node,_)| node)
    .collect::<Vec<_>>();
  for (r,c) in corridors {
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

  // convert: (r,c) hashmap -> index vector
  let indexes = graph.keys().enumerate().map(|(i,pos)| (pos,i)).collect::<HashMap<_,_>>();
  let mut idx_graph = vec![Vec::new(); graph.len()];
  for (pos, neighbors) in &graph {
    idx_graph[indexes[pos]] = neighbors.iter().map(|&(r,c,d)| (indexes[&(r,c)],d)).collect();
  }

  let goal = indexes[&(grid.len()-1, grid[0].len()-2)];
  dfs(&idx_graph, &mut vec![false; idx_graph.len()], goal, indexes[&(0,1)]).unwrap()
}

#[aoc::main(23)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  (solve(&grid, false), solve(&grid, true))
}
