use std::process::Command;
use regex::Regex;

static DAYS: [&str; 25] = [
  "01","02","03","04","05","06","07","08","09","10",
  "11","12","13","14","15","16","17","18","19","20",
  "21","22","23","24","25"
];

fn main() {
  let ms_re = Regex::new(r"Time: (\d+)ms").unwrap();
  let mut total_time = 0;
  for day in &DAYS {
    let cmd = Command::new("cargo")
      .args(&["run", "--release", "--bin", day])
      .output()
      .unwrap();
    let s = String::from_utf8(cmd.stdout).unwrap();
    total_time += ms_re.captures_iter(&s)
      .next()
      .unwrap()[1]
      .parse::<u32>()
      .unwrap();
    println!("Day {}:\n{}", day, s);
  }
  println!("Total time: {}ms", total_time);
}
