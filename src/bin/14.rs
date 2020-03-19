use std::time::Instant;
use std::collections::HashMap;
use std::cmp::Ordering;

const INPUT: [(&str, usize, &[(usize,&str)]); 56] = [
  ("RFSZD", 9, &[(1,"QDKHC")]),
  ("JCHF",  3, &[(15,"FHRN"),(17,"ZFSLM"),(2,"TQFKQ")]),
  ("TQFKQ", 4, &[(4,"KDPV")]),
  ("FNJM",  3, &[(1,"FSTRZ"),(5,"QNXWF"),(2,"RZSD")]),
  ("WQTL",  3, &[(15,"VQPC"),(1,"TXCJ")]),
  ("VPCP",  6, &[(1,"PQCQN"),(6,"HKXPJ"),(16,"ZFSLM"),(6,"SJBPT"),(1,"TKZNJ"),(13,"JBDF"),(1,"RZSD")]),
  ("VNGD",  7, &[(1,"LJGZP")]),
  ("LJGZP", 1, &[(1,"CTVB"),(1,"HVGW")]),
  ("VDKF",  2, &[(6,"HVGW"),(1,"HJWT")]),
  ("CDMX",  3, &[(10,"PQCQN"),(7,"WRQLB"),(1,"XMCH")]),
  ("SJBPT", 4, &[(14,"VNGD"),(23,"ZFSLM"),(2,"FHRN")]),
  ("CKFW",  4, &[(1,"FSTRZ"),(4,"VTWB"),(2,"BLJC")]),
  ("HMLTV", 4, &[(2,"ZTFH"),(19,"CKFW"),(2,"FHRN"),(4,"FNJM"),(9,"NWTVF"),(11,"JBDF"),(1,"VDKF"),(2,"DMRCN")]),
  ("FPMSL", 5, &[(1,"KVZXR")]),
  ("QDKHC", 8, &[(8,"XBZJ")]),
  ("FHRN",  9, &[(1,"VQPC")]),
  ("ZFSLM", 4, &[(15,"RKTFX"),(5,"HKXPJ")]),
  ("QCKFR", 5, &[(1,"HKXPJ"),(8,"LQCTQ"),(21,"VJGKN")]),
  ("KVZXR", 4, &[(1,"DCLQ"),(1,"TQFKQ")]),
  ("JFLQD", 9, &[(4,"NWTVF"),(20,"QNXWF")]),
  ("RZSD",  3, &[(11,"QFVR")]),
  ("JBDF",  7, &[(9,"RFSZD"),(6,"WQTL")]),
  ("QFVR",  8, &[(4,"BLJC"),(3,"LQCTQ"),(1,"QCKFR")]),
  ("VQPC",  5, &[(6,"VNGD")]),
  ("VTWB",  9, &[(7,"CTMR"),(10,"SJBPT")]),
  ("DMRCN", 9, &[(1,"VTWB")]),
  ("FUEL",  1, &[(6,"BCGLR"),(4,"TPTN"),(29,"VNGD"),(25,"KDQC"),(40,"JCHF"),(5,"HMLTV"),(4,"CHWS"),(2,"CDMX"),(1,"VPCP")]),
  ("RKTFX", 6, &[(1,"TQFKQ"),(3,"FPMSL"),(7,"KDPV")]),
  ("WRQLB", 6, &[(8,"HKXPJ"),(2,"WQTL")]),
  ("KDPV",  3, &[(146,"ORE")]),
  ("XMCH",  2, &[(9,"KDQC")]),
  ("CTVB",  4, &[(1,"BGVXG"),(21,"KVZXR"),(1,"LQCTQ")]),
  ("VJGKN", 5, &[(1,"LQCTQ")]),
  ("CTMR",  1, &[(16,"VNGD"),(5,"VMBM")]),
  ("HKXPJ", 5, &[(5,"VCVTM"),(1,"FPMSL")]),
  ("BLJC",  5, &[(4,"HKXPJ")]),
  ("NWTVF", 1, &[(14,"FHRN"),(6,"ZFSLM")]),
  ("VMBM",  7, &[(7,"QCKFR"),(2,"VNGD")]),
  ("QNXWF", 2, &[(4,"TXCJ"),(1,"VDKF")]),
  ("BGVXG", 6, &[(136,"ORE")]),
  ("XBZJ",  9, &[(5,"LQCTQ"),(11,"DCLQ")]),
  ("ZTFH",  7, &[(3,"VQPC")]),
  ("ZWFZX", 3, &[(114,"ORE")]),
  ("TXCJ",  7, &[(1,"HJWT"),(18,"KDPV")]),
  ("VCVTM", 2, &[(1,"VJGKN")]),
  ("HJWT",  1, &[(2,"KVZXR")]),
  ("CHWS",  1, &[(12,"ZWFZX"),(1,"FHRN"),(9,"JFLQD")]),
  ("FSTRZ", 5, &[(3,"QFVR")]),
  ("HVGW",  4, &[(5,"XBZJ")]),
  ("LQCTQ", 8, &[(1,"ZWFZX")]),
  ("KDQC",  9, &[(16,"WQTL"),(10,"TXCJ")]),
  ("TPTN",  5, &[(3,"FHRN"),(12,"LJGZP")]),
  ("PQCQN", 7, &[(1,"JCHF")]),
  ("DCLQ",  7, &[(7,"KDPV"),(17,"BGVXG")]),
  ("BCGLR", 3, &[(1,"CKFW"),(3,"TKZNJ"),(4,"PQCQN"),(1,"VQPC"),(32,"QFVR"),(1,"FNJM"),(13,"FSTRZ")]),
  ("TKZNJ", 5, &[(2,"FSTRZ")]),
];

fn div_round_up(a: usize, b: usize) -> usize { (a + b - 1) / b }

struct Producer {
  store: HashMap<&'static str, usize>,
  costs: HashMap<&'static str, (usize, &'static [(usize, &'static str)])>,
}

impl Producer {
  fn new() -> Self {
    let store = INPUT.iter()
      .map(|&(m,_,_)| (m,0))
      .collect();
    let costs = INPUT.iter()
      .map(|&(m,n,v)| (m, (n, v)))
      .collect();
    Self { store, costs }
  }

  fn produce(&mut self, material: &'static str, needed: usize) -> usize {
    if material == "ORE" { return needed; }

    let (mut ore, mut curr_amount) = (0, self.store[material]);
    if curr_amount < needed {
      let (num_produced, mats) = self.costs[material];
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

fn binary_search(producer: &mut Producer, target: usize) -> usize {
  let (mut min, mut max) = (0,4000000);
  loop {
    producer.reset();
    let mid = (max + min) / 2;
    let ore = producer.produce("FUEL", mid);
    match ore.cmp(&target) {
      Ordering::Less    => min = mid + 1,
      Ordering::Greater => max = mid - 1,
      Ordering::Equal   => unreachable!(),
    }
    if max < min { return mid; }
  };
}

fn main() {
  let now = Instant::now();
  let mut producer = Producer::new();
  let part_one = producer.produce("FUEL", 1);
  let part_two = binary_search(&mut producer, 1_000_000_000_000);
  println!("Part one: {}", part_one);
  println!("Part two: {}", part_two);
  println!("Time: {}ms", now.elapsed().as_millis());
}
