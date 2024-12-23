

#[aoc::main($DAY)]
fn main(input: &str) -> (usize, usize) {
    let xs = input.split('\n').map(|l| {
        l.split(',').collect::<Vec<_>>()
    }).collect::<Vec<_>>();
    (0, 0)
}
