use std::time::Instant;
use std::collections::HashMap;
use std::cmp::Ordering;

macro_rules! input {
  () => {[
    ( (9,"RFSZD"), vec![(1,"QDKHC")] ),
    ( (3,"JCHF"),  vec![(15,"FHRN"),(17,"ZFSLM"),(2,"TQFKQ")] ),
    ( (4,"TQFKQ"), vec![(4,"KDPV")] ),
    ( (3,"FNJM"),  vec![(1,"FSTRZ"),(5,"QNXWF"),(2,"RZSD")] ),
    ( (3,"WQTL"),  vec![(15,"VQPC"),(1,"TXCJ")] ),
    ( (6,"VPCP"),  vec![(1,"PQCQN"),(6,"HKXPJ"),(16,"ZFSLM"),(6,"SJBPT"),(1,"TKZNJ"),(13,"JBDF"),(1,"RZSD")] ),
    ( (7,"VNGD"),  vec![(1,"LJGZP")] ),
    ( (1,"LJGZP"), vec![(1,"CTVB"),(1,"HVGW")] ),
    ( (2,"VDKF"),  vec![(6,"HVGW"),(1,"HJWT")] ),
    ( (3,"CDMX"),  vec![(10,"PQCQN"),(7,"WRQLB"),(1,"XMCH")] ),
    ( (4,"SJBPT"), vec![(14,"VNGD"),(23,"ZFSLM"),(2,"FHRN")] ),
    ( (4,"CKFW"),  vec![(1,"FSTRZ"),(4,"VTWB"),(2,"BLJC")] ),
    ( (4,"HMLTV"), vec![(2,"ZTFH"),(19,"CKFW"),(2,"FHRN"),(4,"FNJM"),(9,"NWTVF"),(11,"JBDF"),(1,"VDKF"),(2,"DMRCN")] ),
    ( (5,"FPMSL"), vec![(1,"KVZXR")] ),
    ( (8,"QDKHC"), vec![(8,"XBZJ")] ),
    ( (9,"FHRN"),  vec![(1,"VQPC")] ),
    ( (4,"ZFSLM"), vec![(15,"RKTFX"),(5,"HKXPJ")] ),
    ( (5,"QCKFR"), vec![(1,"HKXPJ"),(8,"LQCTQ"),(21,"VJGKN")] ),
    ( (4,"KVZXR"), vec![(1,"DCLQ"),(1,"TQFKQ")] ),
    ( (9,"JFLQD"), vec![(4,"NWTVF"),(20,"QNXWF")] ),
    ( (3,"RZSD"),  vec![(11,"QFVR")] ),
    ( (7,"JBDF"),  vec![(9,"RFSZD"),(6,"WQTL")] ),
    ( (8,"QFVR"),  vec![(4,"BLJC"),(3,"LQCTQ"),(1,"QCKFR")] ),
    ( (5,"VQPC"),  vec![(6,"VNGD")] ),
    ( (9,"VTWB"),  vec![(7,"CTMR"),(10,"SJBPT")] ),
    ( (9,"DMRCN"), vec![(1,"VTWB")] ),
    ( (1,"FUEL"),  vec![(6,"BCGLR"),(4,"TPTN"),(29,"VNGD"),(25,"KDQC"),(40,"JCHF"),(5,"HMLTV"),(4,"CHWS"),(2,"CDMX"),(1,"VPCP")] ),
    ( (6,"RKTFX"), vec![(1,"TQFKQ"),(3,"FPMSL"),(7,"KDPV")] ),
    ( (6,"WRQLB"), vec![(8,"HKXPJ"),(2,"WQTL")] ),
    ( (3,"KDPV"),  vec![(146,"ORE")] ),
    ( (2,"XMCH"),  vec![(9,"KDQC")] ),
    ( (4,"CTVB"),  vec![(1,"BGVXG"),(21,"KVZXR"),(1,"LQCTQ")] ),
    ( (5,"VJGKN"), vec![(1,"LQCTQ")] ),
    ( (1,"CTMR"),  vec![(16,"VNGD"),(5,"VMBM")] ),
    ( (5,"HKXPJ"), vec![(5,"VCVTM"),(1,"FPMSL")] ),
    ( (5,"BLJC"),  vec![(4,"HKXPJ")] ),
    ( (1,"NWTVF"), vec![(14,"FHRN"),(6,"ZFSLM")] ),
    ( (7,"VMBM"),  vec![(7,"QCKFR"),(2,"VNGD")] ),
    ( (2,"QNXWF"), vec![(4,"TXCJ"),(1,"VDKF")] ),
    ( (6,"BGVXG"), vec![(136,"ORE")] ),
    ( (9,"XBZJ"),  vec![(5,"LQCTQ"),(11,"DCLQ")] ),
    ( (7,"ZTFH"),  vec![(3,"VQPC")] ),
    ( (3,"ZWFZX"), vec![(114,"ORE")] ),
    ( (7,"TXCJ"),  vec![(1,"HJWT"),(18,"KDPV")] ),
    ( (2,"VCVTM"), vec![(1,"VJGKN")] ),
    ( (1,"HJWT"),  vec![(2,"KVZXR")] ),
    ( (1,"CHWS"),  vec![(12,"ZWFZX"),(1,"FHRN"),(9,"JFLQD")] ),
    ( (5,"FSTRZ"), vec![(3,"QFVR")] ),
    ( (4,"HVGW"),  vec![(5,"XBZJ")] ),
    ( (8,"LQCTQ"), vec![(1,"ZWFZX")] ),
    ( (9,"KDQC"),  vec![(16,"WQTL"),(10,"TXCJ")] ),
    ( (5,"TPTN"),  vec![(3,"FHRN"),(12,"LJGZP")] ),
    ( (7,"PQCQN"), vec![(1,"JCHF")] ),
    ( (7,"DCLQ"),  vec![(7,"KDPV"),(17,"BGVXG")] ),
    ( (3,"BCGLR"), vec![(1,"CKFW"),(3,"TKZNJ"),(4,"PQCQN"),(1,"VQPC"),(32,"QFVR"),(1,"FNJM"),(13,"FSTRZ")] ),
    ( (5,"TKZNJ"), vec![(2,"FSTRZ")] ),
  ]};
}

type Sstr = &'static str;

fn div_round_up(a: usize, b: usize) -> usize { (a + b - 1) / b }

struct Producer {
  store: HashMap<Sstr, usize>,
  costs: HashMap<Sstr, (usize, Vec<(usize, Sstr)>)>,
}

impl Producer {
  fn new() -> Self {
    let input = input!();
    let store = input.iter()
      .map(|((_,m),_)| (*m,0))
      .collect();
    let costs = input.iter()
      .map(|((n,m),v)| (*m, (*n, v.clone())))
      .collect();
    Self { store, costs }
  }

  fn produce(&mut self, material: Sstr, needed: usize) -> usize {
    if material == "ORE" { return needed; }

    let (mut ore, mut curr_amount) = (0, self.store[material]);
    if curr_amount < needed {
      let (num_produced, mats) = self.costs[material].clone();
      let rounds = div_round_up(needed - curr_amount, num_produced);
      ore += mats.iter()
        .map(|(needed, mat)| self.produce(mat, needed * rounds))
        .sum::<usize>();
      curr_amount += num_produced * rounds;
    }

    self.store.insert(material, curr_amount - needed);
    ore
  }

  fn reset(&mut self) {
    for i in self.store.values_mut() { *i = 0; }
  }
}

fn main() {
  let now = Instant::now();
  let mut producer = Producer::new();
  let part_one = producer.produce("FUEL", 1);

  let (mut min, mut max) = (0,4000000);
  let part_two = loop {
    producer.reset();
    let mid = (max + min) / 2;
    let ore = producer.produce("FUEL", mid);
    match ore.cmp(&1000000000000) {
      Ordering::Less    => min = mid + 1,
      Ordering::Greater => max = mid - 1,
      Ordering::Equal   => unreachable!(),
    }
    if max < min { break mid; }
  };
  println!("Part one: {}", part_one);
  println!("Part two: {}", part_two);
  println!("Time: {}ms", now.elapsed().as_millis());
}
