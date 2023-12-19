use hashbrown::HashMap;
use itertools::Itertools;

type WorkFlows<'a> = HashMap<&'a str, (Vec<(char, char, usize, &'a str)>, &'a str)>;

fn filter_p1(workflows: &WorkFlows<'_>, vals: [usize; 4]) -> bool {
  let mut curr = "in";
  while curr != "A" && curr != "R" {
    let workflow = &workflows[curr];
    curr = workflow.0.iter()
      .find(|&&(p, op, n, _)| {
        let i = "xmas".chars().position(|c| c == p).unwrap();
        if op == '<' {vals[i] < n} else {vals[i] > n}
      })
      .map(|&(_, _, _, label)| label)
      .unwrap_or(workflow.1);
  }
  curr == "A"
}

fn count_accepted<'a>(workflows: &WorkFlows<'a>, curr: &'a str, mut ranges: [Vec<usize>; 4]) -> usize {
  if curr == "A" {
    return ranges.iter().map(|v| v.len()).product();
  }
  if curr == "R" {
    return 0;
  }
  let mut ans = 0;
  let workflow = &workflows[curr];
  for &(p, op, n, label) in &workflow.0 {
    let i = "xmas".chars().position(|c| c == p).unwrap();
    let (r, tmp) = ranges[i].iter()
      .partition(|&&val| if op == '<' {val < n} else {val > n});
    let mut newranges = ranges.clone();
    newranges[i] = r;
    ans += count_accepted(workflows, label, newranges);
    ranges[i] = tmp;
  }
  ans += count_accepted(workflows, workflow.1, ranges);
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
        .map(|w| w.parse::<usize>().unwrap())
        .collect_tuple()
        .unwrap()
    )
    .filter(|&(x,m,a,s)| filter_p1(&workflows, [x, m, a, s]))
    .map(|(x,m,a,s)| x + m + a + s)
    .sum();
  let p2 = count_accepted(&workflows, "in", std::array::from_fn(|_| (1..=4000).collect::<Vec<_>>()));
  (p1, p2)
}
