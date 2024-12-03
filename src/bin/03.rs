use regex::Regex;

#[aoc::main(03)]
fn main(input: &str) -> (usize, usize) {
    let r = Regex::new(r"(mul\(\d+,\d+\)|do(n't)?\(\))").unwrap();
    let (mut p1, mut p2, mut ok) = (0, 0, true);
    for x in r.find_iter(input) {
        match x.as_str() {
            "do()" => ok = true,
            "don't()" => ok = false,
            x => {
                let (a, b) = x.strip_prefix("mul(").unwrap().strip_suffix(')').unwrap().split_once(',').unwrap();
                let i = a.parse::<usize>().unwrap() * b.parse::<usize>().unwrap();
                p1 += i;
                if ok {
                    p2 += i;
                }
            }
        }
    }
    (p1, p2)
}
