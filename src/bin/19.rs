use hashbrown::HashMap;
use itertools::Itertools;

type WorkFlows<'a> = HashMap<&'a str, (Vec<(char, char, usize, &'a str)>, &'a str)>;

fn filter_p1(workflows: &WorkFlows<'_>, x: usize, m: usize, a: usize, s: usize) -> bool {
  let mut curr = "in";
  while curr != "A" && curr != "R" {
    let workflow = &workflows[curr];
    curr = workflow.0.iter()
      .find(|&&(p, op, n, _)| {
        let val = match p {
          'x' => x,
          'm' => m,
          'a' => a,
          's' => s,
          _ => unreachable!("{}", p),
        };
        if op == '<' {val < n} else {val > n}
      })
      .map(|&(_, _, _, label)| label)
      .unwrap_or(workflow.1);
  }
  curr == "A"
}

fn count_accepted<'a>(workflows: &WorkFlows<'a>, curr: &'a str, mut x: Vec<usize>, mut m: Vec<usize>, mut a: Vec<usize>, mut s: Vec<usize>) -> usize {
  if curr == "A" {
    return x.len() * m.len() * a.len() * s.len();
  }
  if curr == "R" {
    return 0;
  }
  let mut ans = 0;
  let workflow = &workflows[curr];
  for &(p, op, n, label) in &workflow.0 {
    match p {
      'x' => {
        let (xx, tmp): (Vec<_>, Vec<_>) = x.iter().partition(|&&val| if op == '<' {val < n} else {val > n});
        if !xx.is_empty() {
          ans += count_accepted(workflows, label, xx, m.clone(), a.clone(), s.clone());
        }
        x = tmp;
      }
      'm' => {
        let (mm, tmp): (Vec<_>, Vec<_>) = m.iter().partition(|&&val| if op == '<' {val < n} else {val > n});
        if !mm.is_empty() {
          ans += count_accepted(workflows, label, x.clone(), mm, a.clone(), s.clone());
        }
        m = tmp;
      }
      'a' => {
        let (aa, tmp): (Vec<_>, Vec<_>) = a.iter().partition(|&&val| if op == '<' {val < n} else {val > n});
        if !aa.is_empty() {
          ans += count_accepted(workflows, label, x.clone(), m.clone(), aa, s.clone());
        }
        a = tmp;
      }
      's' => {
        let (ss, tmp): (Vec<_>, Vec<_>) = s.iter().partition(|&&val| if op == '<' {val < n} else {val > n});
        if !ss.is_empty() {
          ans += count_accepted(workflows, label, x.clone(), m.clone(), a.clone(), ss);
        }
        s = tmp;
      }
      _ => unreachable!(),
    }
  }
  ans += count_accepted(workflows, workflow.1, x, m, a, s);
  ans
}

#[aoc::main(19)]
fn main(input: &str) -> (usize, usize) {
  let (workflows, parts) = input.split_once("\n\n").unwrap();
  let workflows = workflows.split('\n').map(|l| {
    let (name, rest) = l.split_once('{').unwrap();
    let (rest, label) = rest[0..rest.len()-1].split_at(rest.rfind(',').unwrap());
    let rules = rest.split(',').map(|rule| {
      let (rest, label) = rule.split_once(':').unwrap();
      let op = if rest.contains('<') {'<'} else {'>'};
      let (name, n) = rest.split_once(op).unwrap();
      (name.as_bytes()[0] as char, op, n.parse::<usize>().unwrap(), label)
    }).collect::<Vec<_>>();
    (name, (rules, &label[1..]))
  }).collect::<HashMap<_,_>>();

  let p1 = parts.split('\n')
    .map(|l|
      l.split(|c: char| !c.is_ascii_digit())
        .filter(|l| !l.is_empty())
        .map(|w| w.parse().unwrap())
        .collect_tuple()
        .unwrap()
    )
    .filter(|&(x,m,a,s)| filter_p1(&workflows, x, m, a, s))
    .map(|(x,m,a,s)| x + m + a + s)
    .sum();
  let init = (1..=4000).collect::<Vec<_>>();
  let p2 = count_accepted(&workflows, "in", init.clone(), init.clone(), init.clone(), init);
  (p1, p2)
}
