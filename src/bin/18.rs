use itertools::Itertools;

fn parse_num(s: &str) -> Vec<(u32,u8)> {
  let (mut d, mut num) = (0, Vec::new());
  for c in s.chars() {
    match c {
      '[' => d += 1,
      ']' => d -= 1,
      ',' => {}
      _   => num.push(((c as u8 - b'0') as u32, d)),
    }
  }
  num
}

fn explode(num: &mut Vec<(u32,u8)>) {
  loop {
    let maxd = *num.iter().map(|(_,d)| d).max().unwrap();
    if maxd < 5 { return }
    let i = (0..num.len()).find(|&i| num[i].1 == maxd).unwrap();
    let ((a,d),b) = (num[i], num.remove(i+1).0);
    if i != 0 { num[i-1].0 += a; }
    if i+1 != num.len() { num[i+1].0 += b; }
    num[i] = (0,d-1);
  }
}

fn split(num: &mut Vec<(u32,u8)>) -> bool {
  let i = match (0..num.len()).find(|&i| num[i].0 > 9) {
    Some(i) => i,
    None => return false,
  };
  let (x,d) = num[i];
  num[i] = (x/2, d+1);
  num.insert(i+1, ((x+1)/2, d+1));
  true
}

fn add(n1: &[(u32,u8)], n2: &[(u32,u8)]) -> Vec<(u32,u8)> {
  let mut num = n1.iter().chain(n2).map(|&(n,d)| (n,d+1)).collect::<Vec<_>>();
  loop {
    explode(&mut num);
    if !split(&mut num) { break }
  }
  num
}

fn magnitude(mut num: Vec<(u32,u8)>) -> u32 {
  while num.len() > 1 {
    let maxd = *num.iter().map(|(_,d)| d).max().unwrap();
    let i = (0..num.len()).filter(|&i| num[i].1 == maxd).min().unwrap();
    let ((n1,d1),(n2,d2)) = (num[i], num[i+1]);
    if d1 == d2 {
      num[i] = (n1*3 + n2*2, d1-1);
      num.remove(i+1);
    }
  }
  num[0].0
}

#[aoc::main("18")]
fn main(input: &str) -> (u32,u32) {
  let nums = input.lines().map(parse_num).collect::<Vec<_>>();
  let p1 = magnitude(nums[1..].iter().fold(nums[0].clone(), |n1,n2| add(&n1,n2)));
  let p2 = nums.iter()
    .tuple_combinations()
    .flat_map(|(n1,n2)| [magnitude(add(n1,n2)), magnitude(add(n2,n1))])
    .max()
    .unwrap();
  (p1,p2)
}
