use std::sync::mpsc::{channel, Sender};
use std::process::Command;
use std::thread;

fn day_str(day: usize) -> String {
  let prefix = if day < 9 {"0"} else {""};
  format!("{}{}", prefix, day + 1)
}

fn execute_day(tx: Sender<(usize,String)>, day: usize) {
  thread::spawn(move || {
    let cmd = Command::new("cargo")
      .args(&["run", "--release", "--bin", &day_str(day)])
      .output()
      .unwrap();
    let s = String::from_utf8(cmd.stdout).unwrap();
    tx.send((day, s)).unwrap();
  });
}

fn main() {
  let (tx,rx) = channel();
  for day in 0..25 {
    execute_day(tx.clone(), day);
  }

  let mut v = vec![String::new(); 25];
  for _ in 0..25 {
    let (day,s) = rx.recv().unwrap();
    v[day] = s;
  }
  for day in 0..25 {
    println!("Day {}:\n{}", day+1, v[day]);
  }
}
