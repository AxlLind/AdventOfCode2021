use hashbrown::HashMap;

const ROCKS: [&[(usize, usize)]; 5] = [
  &[(0,0),(0,1),(0,2),(0,3)],
  &[(0,1),(1,0),(1,2),(2,1)],
  &[(0,0),(0,1),(0,2),(1,2),(2,2)],
  &[(0,0),(1,0),(2,0),(3,0)],
  &[(0,0),(0,1),(1,0),(1,1)],
];

fn get_height(map: &[[u8; 7]]) -> usize {
  map.iter().position(|row| row == &[0;7]).unwrap()
}

fn can_fit(map: &[[u8; 7]], rock: &[(usize, usize)], h: usize, w: usize) -> bool {
  rock.iter().all(|(dh,dw)| w + dw < 7 && map[h + dh][w + dw] != b'#')
}

fn column_heights(map: &[[u8; 7]]) -> [usize; 7] {
  let mut heights = [0; 7];
  let h = get_height(map);
  for i in 0..7 {
    heights[i] = (0..h).find(|&x| map[h - x][i] == b'#').unwrap_or(usize::MAX);
  }
  heights
}

#[aoc::main(17)]
fn main(input: &str) -> (usize, usize) {
  let mut map = [[0; 7]; 10000];
  let (mut i, mut t, mut total_height) = (0, 0, 0);
  let mut cache = HashMap::new();
  while i < 1000000000000 {
    let rock = ROCKS[i % ROCKS.len()];
    let (mut h, mut w) = (get_height(&map) + 3, 2);
    loop {
      match input.as_bytes()[t % input.len()] {
        b'>' => if can_fit(&map, rock, h, w + 1) { w += 1 },
        b'<' => if can_fit(&map, rock, h, w - 1) { w -= 1 },
        _ => {},
      };
      t += 1;
      if h == 0 || !can_fit(&map, rock, h-1, w) {
        break;
      }
      h -= 1;
    }
    for (dh, dw) in rock { map[h+dh][w+dw] = b'#' }

    let key = (i % ROCKS.len(), t % input.len(), column_heights(&map));
    if let Some((idx, height)) = cache.get(&key) {
      let repeats = (1000000000000 - idx) / (i - idx) - 1;
      i += (i - idx) * repeats;
      total_height += (get_height(&map) - height) * repeats;
    } else {
      cache.insert(key, (i, get_height(&map)));
    }
    i += 1;
  }
  let p1 = *cache.values().find(|&&(i,_)| i == 2021).map(|(_,h)| h).unwrap();
  (p1, total_height + get_height(&map))
}
