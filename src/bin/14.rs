use std::collections::HashMap;
use itertools::Itertools;

static START: &str = "OOBFPNOPBHKCCVHOBCSO";
static INGREDIENTS: &str = "NS -> H\nNN -> P\nFF -> O\nHF -> C\nKN -> V\nPO -> B\nPS -> B\nFB -> N\nON -> F\nOK -> F\nOO -> K\nKS -> F\nFN -> F\nKC -> H\nNC -> N\nNB -> C\nKH -> S\nSV -> B\nBC -> S\nKB -> B\nSC -> S\nKP -> H\nFS -> K\nNK -> K\nOC -> H\nNH -> C\nPH -> F\nOS -> V\nBB -> C\nCC -> F\nCF -> H\nCP -> V\nVB -> N\nVC -> F\nPK -> V\nNV -> N\nFO -> S\nCK -> O\nBH -> K\nPN -> B\nPP -> S\nNF -> B\nSF -> K\nVF -> H\nHS -> F\nNP -> F\nSH -> V\nSK -> K\nPC -> V\nBO -> H\nHN -> P\nBK -> O\nBP -> S\nOP -> N\nSP -> N\nKK -> C\nHB -> H\nOF -> C\nVH -> C\nHO -> N\nFK -> V\nNO -> H\nKF -> S\nKO -> V\nPF -> K\nHV -> C\nSO -> F\nSS -> F\nVN -> K\nHH -> B\nOB -> S\nCH -> B\nFH -> B\nCO -> V\nHK -> F\nVK -> K\nCN -> V\nSB -> K\nPV -> O\nPB -> F\nVV -> P\nCS -> N\nCB -> C\nBS -> F\nHC -> B\nSN -> P\nVP -> P\nOV -> P\nBV -> P\nFC -> N\nKV -> S\nCV -> F\nBN -> S\nBF -> C\nOH -> F\nVO -> B\nFP -> S\nFV -> V\nVS -> N\nHP -> B";

fn simulate_polymer(recipies: &HashMap<(char,char),char>, steps: usize) -> usize {
  let init_count = START.chars().tuple_windows().counts();
  let pair_counts = (0..steps).fold(init_count, |counts, _| {
    let mut next_counts = HashMap::new();
    for (&(a,b),count) in &counts {
      let c = recipies[&(a,b)];
      *next_counts.entry((a,c)).or_insert(0) += count;
      *next_counts.entry((c,b)).or_insert(0) += count;
    }
    next_counts
  });
  let mut count = HashMap::new();
  for (&(a,_), c) in &pair_counts {
    *count.entry(a).or_insert(0) += c;
  }
  *count.entry(START.chars().last().unwrap()).or_insert(0) += 1;
  let (min,max) = count.values().minmax().into_option().unwrap();
  max - min
}

aoc2021::main! {
  let recipies = INGREDIENTS.lines()
    .map(|l| {
      let (a,b) = l.split_once(" -> ").unwrap();
      let k = (a.as_bytes()[0] as char, a.as_bytes()[1] as char);
      (k, b.as_bytes()[0] as char)
    })
    .collect::<HashMap<_,_>>();
  let p1 = simulate_polymer(&recipies, 10);
  let p2 = simulate_polymer(&recipies, 40);
  (p1,p2)
}
