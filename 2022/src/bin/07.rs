use std::path::PathBuf;
use hashbrown::{HashMap, HashSet};

fn compute_dir_size(fs: &HashMap<PathBuf, HashSet<(i64, &str)>>, sizes: &mut HashMap<PathBuf, i64>, dir: &PathBuf) {
  if sizes.contains_key(dir) {
    return;
  }
  let size = fs[dir].iter().map(|&(s, d)| match s {
    -1 => {
      let dir = dir.join(d);
      compute_dir_size(fs, sizes, &dir);
      sizes[&dir]
    }
    s => s,
  }).sum();
  sizes.insert(dir.clone(), size);
}

#[aoc::main(07)]
fn main(input: &str) -> (i64, i64) {
  let mut fs = HashMap::<_, HashSet<_>>::new();
  let mut pwd = PathBuf::new();
  for l in input.split('$').skip(1) {
    match l.trim().lines().next().unwrap() {
      "ls" => {
        let entries = l.lines().skip(1).map(|output| {
          let (size, f) = output.split_once(' ').unwrap();
          (size.parse::<i64>().unwrap_or(-1), f)
        });
        fs.entry(pwd.clone()).or_default().extend(entries);
      }
      "cd .." => { pwd.pop(); },
      cd_dir => { pwd.push(cd_dir.split_once(' ').unwrap().1); }
    }
  }

  let mut sizes = HashMap::new();
  for k in fs.keys() {
    compute_dir_size(&fs, &mut sizes, k);
  }
  let total_size = sizes[&PathBuf::from("/")];
  let p1 = sizes.values().filter(|&&s| s <= 100000).sum::<i64>();
  let p2 = sizes.values().filter(|&&s| 40000000 + s >= total_size).min().copied().unwrap();
  (p1, p2)
}
