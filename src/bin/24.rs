use hashbrown::HashSet;

static INPUT: &str = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 16\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 14\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 0\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -8\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 1\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -16\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -4\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -3\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 12\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -7\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 3\nmul y x\nadd z y";

// Decompiling each block by hand we get:
//   00:  inp w      ; [I, 0, 0, Z]
//   01:  mul x 0
//   02:  add x z    ; [I, Z, 0, Z]
//   03:  mod x 26   ; [I, Z%26, 0, Z]
//   04:  div z <A>  ; [I, Z%26, 0, Z/A]
//   05:  add x <B>  ; [I, Z%26 + B, 0, Z/A]
//   06:  eql x w
//   07:  eql x 0    ; [I, (Z%26 + B) != I), 0, Z/A]
//   08:  mul y 0
//   09:  add y 25   ; [I, (Z%26 + B) != I, 25, Z/A]
//   10:  mul y x    ; [I, (Z%26 + B) != I, 25 * X, Z/A]
//   11:  add y 1    ; [I, (Z%26 + B) != I, 25 * X + 1, Z/A]
//   12:  mul z y    ; [I, (Z%26 + B) != I, 25 * X + 1, (Z/A) * (25 * X + 1)]
//   13:  mul y 0
//   14:  add y w    ; [I, (Z%26 + B) != I, I, (Z/A) * (25 * X + 1)]
//   15:  add y <C>  ; [I, (Z%26 + B) != I, I + C, (Z/A) * (25 * X + 1)]
//   16:  mul y x    ; [I, (Z%26 + B) != I, (I + C) * X, (Z/A) * (25 * X + 1)]
//   17:  add z y    ; [I, (Z%26 + B) != I, (I + C) * X, (Z/A) * (25 * X + 1) + (I + C) * X]
// => X = (Z%26 + B) != I
//    Z = (Z/A) * (25 * X + 1) + (I + C) * X

fn find_modelnum(cache: &mut HashSet<(usize,i64)>, blocks: &[(i64,i64,i64)], range: &[i64], block: usize, z: i64) -> Option<i64> {
  if block == blocks.len() {
    return if z == 0 {Some(0)} else {None};
  }
  if cache.contains(&(block,z)) { return None; }
  let (a,b,c) = blocks[block];
  for &i in range {
    let z = if z % 26 + b == i {z/a} else {(z/a) * 26 + i + c};
    if let Some(n) = find_modelnum(cache, blocks, range, block+1, z) {
      return Some(i * 10i64.pow(13 - block as u32) + n);
    }
  }
  cache.insert((block,z));
  None
}

aoc2021::main! {
  let lines = INPUT.lines().collect::<Vec<_>>();
  let blocks = lines.chunks(18)
    .map(|block| {
      let a = block[04][6..].parse().unwrap();
      let b = block[05][6..].parse().unwrap();
      let c = block[15][6..].parse().unwrap();
      (a,b,c)
    })
    .collect::<Vec<_>>();
  let p1 = find_modelnum(&mut HashSet::new(), &blocks, &[9,8,7,6,5,4,3,2,1], 0, 0).unwrap();
  let p2 = find_modelnum(&mut HashSet::new(), &blocks, &[1,2,3,4,5,6,7,8,9], 0, 0).unwrap();
  (p1,p2)
}
