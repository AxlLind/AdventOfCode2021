#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Move { L, R, F(usize) }

fn make_move_p1(grid: &[Vec<u8>], mut r: usize, mut c: usize, dir: char, len: usize) -> (usize, usize, char) {
  let (dr, dc) = match dir {
    'U' => (-1, 0),
    'R' => ( 0, 1),
    'D' => ( 1, 0),
    'L' => ( 0,-1),
    _ => unreachable!(),
  };
  for _ in 0..len {
    let next_tile = grid.get(r + dr as usize).and_then(|row| row.get(c + dc as usize)).unwrap_or(&b' ');
    match next_tile {
      b'.' => {
        r += dr as usize;
        c += dc as usize;
      },
      b'#' => break,
      b' ' => {
        let (mut r2, mut c2) = (r, c);
        while *grid.get(r2 - dr as usize).and_then(|row| row.get(c2 - dc as usize)).unwrap_or(&b' ') != b' ' {
          r2 -= dr as usize;
          c2 -= dc as usize;
        }
        let next_tile = *grid.get(r2).and_then(|row| row.get(c2)).unwrap();
        if next_tile == b'#' {
          break;
        }
        (r, c) = (r2, c2);
      }
      _ => unreachable!(),
    }
  }
  (r, c, dir)
}

fn wrap(r: usize, c: usize, dir: char) -> (char, usize, usize) {
  let (qr, qc) = (r / 50, c / 50);
  let (dr, dc) = (r - qr * 50, c - qc * 50);
  let (qr, qc, ndir) = match (qr, qc, dir) {
    (2,1,'U') => (1,1,'U'),
    (2,1,'R') => (0,2,'L'),
    (2,1,'D') => (3,0,'L'),
    (2,1,'L') => (2,0,'L'),
    (0,1,'U') => (3,0,'R'),
    (0,1,'R') => (0,2,'R'),
    (0,1,'D') => (1,1,'D'),
    (0,1,'L') => (2,0,'R'),
    (1,1,'U') => (0,1,'U'),
    (1,1,'R') => (0,2,'U'),
    (1,1,'D') => (2,1,'D'),
    (1,1,'L') => (2,0,'D'),
    (3,0,'U') => (2,0,'U'),
    (3,0,'R') => (2,1,'U'),
    (3,0,'D') => (0,2,'D'),
    (3,0,'L') => (0,1,'D'),
    (0,2,'U') => (3,0,'U'),
    (0,2,'R') => (2,1,'L'),
    (0,2,'D') => (1,1,'L'),
    (0,2,'L') => (0,1,'L'),
    (2,0,'U') => (1,1,'R'),
    (2,0,'R') => (2,1,'R'),
    (2,0,'D') => (3,0,'D'),
    (2,0,'L') => (0,1,'R'),
    _ => unreachable!()
  };
  let x = match dir {
    'U' => dc,
    'R' => dr,
    'D' => 49-dc,
    'L' => 49-dr,
    _ => unreachable!(),
  };
  let (nr, nc) = match ndir {
    'U' => (49,x),
    'R' => (x,0),
    'D' => (0,49-x),
    'L' => (49-x,49),
    _ => unreachable!(),
  };
  (ndir, qr * 50 + nr, qc * 50 + nc)
}

fn make_move_p2(grid: &[Vec<u8>], mut r: usize, mut c: usize, mut dir: char, len: usize) -> (usize, usize, char) {
  let (dr, dc) = match dir {
    'U' => (-1, 0),
    'R' => ( 0, 1),
    'D' => ( 1, 0),
    'L' => ( 0,-1),
    _ => unreachable!(),
  };
  for _ in 0..len {
    let next_tile = grid.get(r + dr as usize).and_then(|row| row.get(c + dc as usize)).unwrap_or(&b' ');
    match next_tile {
      b'.' => {
        r += dr as usize;
        c += dc as usize;
      },
      b'#' => break,
      b' ' => {
        let (d, r2, c2) = wrap(r, c, dir);
        if grid[r2][c2] == b'#' {
          break;
        }
        assert_eq!(grid[r2][c2], b'.');
        (dir, r, c) = (d, r2, c2);
      }
      _ => unreachable!(),
    }
  }
  (r, c, dir)
}

fn walk(map: &[Vec<u8>], moves: &[Move], make_move: impl Fn(&[Vec<u8>], usize, usize, char, usize) -> (usize, usize, char)) -> usize {
  let (mut r, mut c, mut dir) = (0, 0, 'R');
  while map[0][c] != b'.' { c += 1 }
  for &m in moves {
    match m {
      Move::L => dir = match dir {
        'U' => 'L',
        'R' => 'U',
        'D' => 'R',
        'L' => 'D',
        _ => unreachable!(),
      },
      Move::R => dir = match dir {
        'U' => 'R',
        'R' => 'D',
        'D' => 'L',
        'L' => 'U',
        _ => unreachable!(),
      },
      Move::F(len) => (r, c, dir) = make_move(&map, r, c, dir, len),
    }
  }
  (r + 1) * 1000 + (c + 1) * 4 + match dir {
    'U' => 3,
    'R' => 0,
    'D' => 1,
    'L' => 2,
    _ => unreachable!(),
  }
}

#[aoc::main(22)]
fn main(input: &str) -> (usize, usize) {
  let (grid_str, move_str) = input.split_once("\n\n").unwrap();
  let map = grid_str.lines().map(|l| l.as_bytes().to_vec()).collect::<Vec<_>>();
  let mut moves = vec![];
  let mut chars = move_str.chars().peekable();
  loop {
    let m = match chars.next() {
      Some('L') => Move::L,
      Some('R') => Move::R,
      Some(c) => {
        let mut s = c.to_string();
        while let Some(c) =  chars.peek() {
          if !c.is_ascii_digit() {
            break;
          }
          s.push(chars.next().unwrap());
        }
        Move::F(s.parse().unwrap())
      }
      None => break,
    };
    moves.push(m);
  }
  (walk(&map, &moves, make_move_p1), walk(&map, &moves, make_move_p2))
}
