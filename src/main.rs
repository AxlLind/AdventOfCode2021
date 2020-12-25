use std::process::Command;

static DAYS: [&str; 25] = [
  "01","02","03","04","05","06","07","08","09","10",
  "11","12","13","14","15","16","17","18","19","20",
  "21","22","23","24","25"
];

fn main() {
  for day in &DAYS {
    let cmd = Command::new("cargo")
      .args(&["run", "--release", "--bin", day])
      .output()
      .unwrap();
    let s = String::from_utf8(cmd.stdout).unwrap();
    println!("Day {}:\n{}", day, s);
  }
}
