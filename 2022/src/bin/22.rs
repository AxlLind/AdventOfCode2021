const DIR: [(isize, isize); 4] = [(-1,0),(0,1),(1,0),(0,-1)];

fn wrap1(map: &[Vec<u8>], mut r: usize, mut c: usize, dir: usize) -> (usize, usize, usize) {
  let (dr, dc) = DIR[dir];
  while *map.get(r - dr as usize).and_then(|row| row.get(c - dc as usize)).unwrap_or(&b' ') != b' ' {
    (r, c) = (r - dr as usize, c - dc as usize);
  }
  (r, c, dir)
}

fn wrap2(_: &[Vec<u8>], r: usize, c: usize, dir: usize) -> (usize, usize, usize) {
  let (qr, qc, ndir) = match (r / 50, c / 50, dir) {
    (0,1,0) => (3,0,1),
    (0,1,3) => (2,0,1),
    (0,2,0) => (3,0,0),
    (0,2,1) => (2,1,3),
    (0,2,2) => (1,1,3),
    (1,1,1) => (0,2,0),
    (1,1,3) => (2,0,2),
    (2,0,0) => (1,1,1),
    (2,0,3) => (0,1,1),
    (2,1,1) => (0,2,3),
    (2,1,2) => (3,0,3),
    (3,0,1) => (2,1,0),
    (3,0,2) => (0,2,2),
    (3,0,3) => (0,1,2),
    _ => unreachable!(),
  };
  let (dr, dc) = (r % 50, c % 50);
  let i = [dc, dr, 49-dc, 49-dr][dir];
  let (nr, nc) = [(49,i), (i,0), (0,49-i), (49-i,49)][ndir];
  (qr * 50 + nr, qc * 50 + nc, ndir)
}

fn walk(map: &[Vec<u8>], moves: &str, wrap: impl Fn(&[Vec<u8>], usize, usize, usize) -> (usize, usize, usize)) -> usize {
  let (mut r, mut c, mut dir) = (0, 0, 1);
  while map[0][c] != b'.' { c += 1 }
  let mut chars = moves.chars().peekable();
  while let Some(chr) = chars.next() {
    match chr {
      'L' => dir = (dir + 3) % 4,
      'R' => dir = (dir + 1) % 4,
      chr => {
        let mut s = chr.to_string();
        while let Some(chr) = chars.peek() {
          if !chr.is_ascii_digit() { break }
          s.push(chars.next().unwrap());
        }
        for _ in 0..s.parse().unwrap() {
          let (dr, dc) = DIR[dir];
          match map.get(r + dr as usize).and_then(|row| row.get(c + dc as usize)).unwrap_or(&b' ') {
            b'.' => (r, c) = (r + dr as usize, c + dc as usize),
            b'#' => break,
            b' ' => {
              let (nr, nc, d) = wrap(map, r, c, dir);
              if map[nr][nc] == b'#' { break }
              (r, c, dir) = (nr, nc, d);
            },
            _ => unreachable!(),
          }
        }
      }
    }
  }
  1000*(r+1) + 4*(c+1) + [3,0,1,2][dir]
}

#[aoc::main(22)]
fn main(input: &str) -> (usize, usize) {
  let (grid_str, moves) = input.split_once("\n\n").unwrap();
  let map = grid_str.lines().map(|l| l.as_bytes().to_vec()).collect::<Vec<_>>();
  (walk(&map, moves, wrap1), walk(&map, moves, wrap2))
}
