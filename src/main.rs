use std::{fs, process::Command, path::Path};
use itertools::Itertools;

static BIN_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/bin");

fn extract_microseconds(output: &str) -> usize {
  let out = output.lines().last().unwrap();
  if out.ends_with("ms") {
    out["Time: ".len()..out.len()-2].parse::<usize>().unwrap() * 1000
  } else {
    out["Time: ".len()..out.len()-3].parse::<usize>().unwrap()
  }
}

fn main() {
  let total_time = fs::read_dir(&Path::new(BIN_DIR))
    .unwrap()
    .filter_map(|p| Some(p.ok()?.path().file_stem()?.to_str()?.to_string()))
    .sorted()
    .map(|day| {
      let cmd = Command::new("cargo")
        .args(&["run", "--release", "--bin", &day])
        .output()
        .unwrap();
      let output = String::from_utf8(cmd.stdout).unwrap();
      println!("Day {}:\n{}", day, output);
      extract_microseconds(&output)
    })
    .sum::<usize>();
  println!("Total time: {}ms", total_time / 1000);
}
