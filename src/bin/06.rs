use itertools::Itertools;

fn find_unique_chunk(input: &str, size: usize) -> usize {
  input.as_bytes()
    .windows(size)
    .enumerate()
    .find(|(_, window)| window.iter().tuple_combinations().all(|(a,b)| a != b))
    .unwrap().0 + size
}

#[aoc::main(06)]
fn main(input: &str) -> (usize, usize) {
  (find_unique_chunk(input, 4), find_unique_chunk(input, 14))
}
