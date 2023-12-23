use hashbrown::HashMap;

fn dfs(
  graph: &HashMap<(usize,usize), Vec<(usize,usize,usize)>>,
  seen: &mut Vec<Vec<bool>>,
  (r,c): (usize,usize),
  dist: usize,
  max_dist: &mut usize,
) {
  if r == seen.len() - 1 {
    *max_dist = (*max_dist).max(dist);
    return;
  }
  for &(rr, cc, d) in &graph[&(r,c)] {
    if !seen[rr][cc] {
      seen[rr][cc] = true;
      dfs(graph, seen, (rr, cc), dist + d, max_dist);
      seen[rr][cc] = false;
    }
  }
}

fn solve(grid: &[&[u8]], part2: bool) -> usize {
  let mut graph = HashMap::<_,Vec<_>>::new();
  for r in 0..grid.len() {
    for c in 0..grid[0].len() {
      if grid[r][c] == b'#' {
        continue;
      }
      let neighbours: &[_] = match grid[r][c] {
        _ if part2 => &[(-1,0),(1,0),(0,-1),(0,1)],
        b'.' => &[(-1,0),(1,0),(0,-1),(0,1)],
        b'^' => &[(-1,0)],
        b'>' => &[(0,1)],
        b'v' => &[(1,0)],
        b'<' => &[(0,-1)],
        _ => unreachable!(),
      };
      let e = graph.entry((r,c)).or_default();
      for (dr, dc) in neighbours {
        let rr = (r as isize + dr) as usize;
        let cc = (c as isize + dc) as usize;
        let Some(&tile) = grid.get(rr).and_then(|row| row.get(cc)) else {continue};
        if tile != b'#' {
          e.push((rr,cc,1));
        }
      }
    }
  }
  loop {
    let Some((&(r,c), _)) = graph.iter().find(|(_,n)| n.len() == 2) else { break };
    let neighbours = graph.remove(&(r,c)).unwrap();
    let (r1,c1,d1) = neighbours[0];
    let (r2,c2,d2) = neighbours[1];
    let n1 = graph.entry((r1,c1)).or_default();
    if let Some(i) = n1.iter().position(|&(rr,cc,_)| (rr,cc) == (r,c)) {
      n1[i] = (r2,c2,d1+d2);
    }
    let n2 = graph.entry((r2,c2)).or_default();
    if let Some(i) = n2.iter().position(|&(rr,cc,_)| (rr,cc) == (r,c)) {
      n2[i] = (r1,c1,d1+d2);
    }
  }
  let mut seen = vec![vec![false; grid[0].len()]; grid.len()];
  let mut ans = 0;
  dfs(&graph, &mut seen, (0,1), 0, &mut ans);
  ans
}

#[aoc::main(23)]
fn main(input: &str) -> (usize, usize) {
  let grid = input.split('\n').map(str::as_bytes).collect::<Vec<_>>();
  (solve(&grid, false), solve(&grid, true))
}
