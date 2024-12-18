#![allow(unused_assignments)]
use itertools::Itertools;
use z3::ast::{Ast, BV};

fn part1((mut a, mut b, mut c): (i64, i64, i64)) -> String {
    let mut output = Vec::new();
    while a != 0{
        b = a % 8;
        b = b ^ 5;
        c = a / (1 << b);
        b = b ^ c;
        b = b ^ 6;
        a = a / (1 << 3);
        output.push(b % 8);
    }
    output.iter().map(|x| x.to_string()).join(",")
}

#[aoc::main(17)]
fn main(input: &str) -> (String, i64) {
    let mut ints = input
        .split(|c: char| !c.is_ascii_digit() && c != '-')
        .filter(|w| !w.is_empty())
        .map(|w| w.parse::<i64>().unwrap());
    let start_state = ints.next_tuple().unwrap();
    let insts = ints.collect::<Vec<_>>();
    assert_eq!(insts, [2,4,1,5,7,5,4,3,1,6,0,3,5,5,3,0]);
    let p1 = part1(start_state);

    let ctx = z3::Context::new(&z3::Config::new());
    let opt = z3::Optimize::new(&ctx);
    let s = BV::new_const(&ctx, "s", 64);
    let (mut a, mut b, mut c) = (s.clone(), BV::from_i64(&ctx, 0, 64), BV::from_i64(&ctx, 0, 64));
    for x in [2,4,1,5,7,5,4,3,1,6,0,3,5,5,3,0] {
        b = &a & &BV::from_i64(&ctx, 7, 64);
        b = b ^ &BV::from_i64(&ctx, 5, 64);
        c = a.bvlshr(&b);
        b = b ^ c;
        b = b ^ &BV::from_i64(&ctx, 6, 64);
        a = a.bvlshr(&BV::from_i64(&ctx, 3, 64));
        opt.assert(&(&b & &BV::from_i64(&ctx, 7, 64))._eq(&BV::from_i64(&ctx, x, 64)));
    }
    opt.assert(&(a._eq(&BV::from_i64(&ctx, 0, 64))));
    opt.minimize(&s);
    assert_eq!(opt.check(&[]), z3::SatResult::Sat);
    let res = opt.get_model().unwrap().eval(&s, true).unwrap().as_i64().unwrap();
    (p1, res)
}
