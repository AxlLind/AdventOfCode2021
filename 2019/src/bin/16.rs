use std::time::Instant;
use itertools::Itertools;

const INPUT: &str = "59723517898690342336085619027921111260000667417052529433894092649779685557557996383085708903241535436786723718804155370155263736632861535632645335233170435646844328735934063129720822438983948765830873108060969395372667944081201020154126736565212455403582565814037568332106043336657972906297306993727714730061029321153984390658949013821918352341503629705587666779681013358053312990709423156110291835794179056432958537796855287734217125615700199928915524410743382078079059706420865085147514027374485354815106354367548002650415494525590292210440827027951624280115914909910917047084328588833201558964370296841789611989343040407348115608623432403085634084";

fn part_one() -> String {
  let mut phase = INPUT.chars()
    .map(|c| (c as u8 - b'0') as i32)
    .collect_vec();
  for _ in 0..100 {
    for i in 1..=phase.len() {
      let mut index = i-1;
      let mut out = [0,0];
      while index < phase.len() {
        for o in 0..2 {
          for _ in 0..i {
            if index >= phase.len() { break; }
            out[o] += phase[index];
            index += 1;
          }
          index += i;
        }
      }
      phase[i-1] = (out[0] - out[1]).abs() % 10;
    }
  }
  phase[..8].iter()
    .map(|&i| (i as u8 + b'0') as char)
    .collect::<String>()
}

fn part_two() -> String {
  let offset: usize = INPUT[0..7].parse().unwrap();
  let mut phase = INPUT.chars()
    .cycle()
    .skip(offset % INPUT.len())
    .take(INPUT.len() * 10000 - offset)
    .map(|c| (c as u8 - b'0') as i32)
    .collect_vec();
  phase.reverse();
  for _ in 0..100 {
    let mut acc = 0;
    for i in 0..phase.len() {
      acc += phase[i];
      phase[i] = acc % 10;
    }
  }
  phase.iter()
    .rev()
    .take(8)
    .map(|&i| (i as u8 + b'0') as char)
    .collect::<String>()
}

fn main() {
  let now = Instant::now();
  println!("Part one: {}", part_one());
  println!("Part two: {}", part_two());
  println!("Time: {}ms", now.elapsed().as_millis());
}
