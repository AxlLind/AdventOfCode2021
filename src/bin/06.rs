// d = (t - w) * w  =>  w = (t +- sqrt(t^2 - 4d)) / 2
fn f(t: usize, d: usize) -> usize {
  let diff = ((t*t - 4*d) as f64).sqrt();
  let l = (t as f64 - diff) / 2.0;
  let h = (t as f64 + diff) / 2.0;
  (h.floor() - l.ceil()) as usize + 1
}

#[aoc::main(06)]
fn main(input: &str) -> (usize, usize) {
  let (l1, l2) = input.split_once('\n').unwrap();
  let times = l1.split_whitespace().skip(1).map(|w| w.parse().unwrap()).collect::<Vec<_>>();
  let dists = l2.split_whitespace().skip(1).map(|w| w.parse().unwrap()).collect::<Vec<_>>();
  let time2 = l1.split_whitespace().skip(1).collect::<String>().parse().unwrap();
  let dist2 = l2.split_whitespace().skip(1).collect::<String>().parse().unwrap();
  let p1 = times.iter().zip(dists).map(|(&t, d)| f(t, d)).product();
  (p1, f(time2, dist2))
}
