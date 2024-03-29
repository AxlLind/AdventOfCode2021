fn line_score(s: &str) -> (Vec<char>, Option<usize>) {
  let mut stack = Vec::new();
  let score = s.chars().find_map(|c| {
    match c {
      '('|'{'|'['|'<' => stack.push(c),
      ')' => if stack.pop() != Some('(') { return Some(3) }
      ']' => if stack.pop() != Some('[') { return Some(57) }
      '}' => if stack.pop() != Some('{') { return Some(1197) }
      '>' => if stack.pop() != Some('<') { return Some(25137) }
      _ => unreachable!(),
    }
    None
  });
  (stack, score)
}

fn fixed_score(stack: &[char]) -> usize {
  stack.iter().rev().fold(0, |s,c| match c {
    '(' => s * 5 + 1,
    '[' => s * 5 + 2,
    '{' => s * 5 + 3,
    '<' => s * 5 + 4,
    _ => unreachable!(),
  })
}

#[aoc::main(10)]
fn main(input: &str) -> (usize,usize) {
  let (mut p1, mut fixed_scores) = (0, Vec::new());
  for (stack,score) in input.lines().map(line_score) {
    match score {
      Some(score) => p1 += score,
      None => fixed_scores.push(fixed_score(&stack)),
    }
  }
  fixed_scores.sort_unstable();
  (p1, fixed_scores[fixed_scores.len() / 2])
}
