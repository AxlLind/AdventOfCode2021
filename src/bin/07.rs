use std::path::PathBuf;
use hashbrown::{HashMap, HashSet};

fn compute_dir_size<'a>(fs: &HashMap<PathBuf, HashSet<(i64, &'a str)>>, sizes: &mut HashMap<PathBuf, i64>, dir: &PathBuf) {
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
  let mut fs = HashMap::new();
  let mut pwd = PathBuf::new();
  for l in input.split("$").skip(1).map(|l| l.trim()) {
    let cmd = l.lines().next().unwrap();
    match cmd.trim() {
      "ls" => {
        let entries = l.lines()
          .skip(1)
          .map(|output| {
            let (x, f) = output.split_once(' ').unwrap();
            let size = if x == "dir" {-1} else {x.parse::<i64>().unwrap()};
            (size, f)
          });
        fs.entry(pwd.clone()).or_insert(HashSet::new()).extend(entries);
      }
      "cd .." => { pwd.pop(); },
      _ => { pwd.push(cmd.split_once(' ').unwrap().1); }
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
