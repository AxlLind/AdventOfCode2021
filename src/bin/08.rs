enum Op {
  Acc(i32),
  Jmp(i32),
  Nop(i32),
}

const INPUT_LEN: usize = 623;
static INPUT: [Op; INPUT_LEN] = [Op::Acc(13), Op::Acc(-6), Op::Acc(-8), Op::Jmp(140), Op::Acc(44), Op::Acc(21), Op::Nop(23), Op::Jmp(455), Op::Acc(-1), Op::Jmp(143), Op::Acc(9), Op::Acc(19), Op::Jmp(507), Op::Nop(513), Op::Acc(38), Op::Nop(444), Op::Jmp(560), Op::Nop(19), Op::Acc(9), Op::Acc(19), Op::Jmp(33), Op::Acc(11), Op::Acc(-11), Op::Acc(10), Op::Jmp(486), Op::Nop(-12), Op::Acc(38), Op::Acc(5), Op::Jmp(394), Op::Acc(23), Op::Jmp(236), Op::Acc(-9), Op::Acc(-10), Op::Acc(32), Op::Nop(45), Op::Jmp(562), Op::Jmp(423), Op::Acc(3), Op::Nop(340), Op::Jmp(217), Op::Acc(-14), Op::Acc(-6), Op::Jmp(397), Op::Acc(17), Op::Nop(165), Op::Acc(41), Op::Acc(-9), Op::Jmp(554), Op::Nop(7), Op::Acc(0), Op::Jmp(235), Op::Acc(32), Op::Jmp(486), Op::Jmp(280), Op::Jmp(408), Op::Jmp(73), Op::Jmp(482), Op::Acc(-17), Op::Acc(24), Op::Jmp(377), Op::Jmp(379), Op::Acc(13), Op::Jmp(277), Op::Nop(232), Op::Acc(2), Op::Acc(33), Op::Jmp(247), Op::Acc(48), Op::Acc(22), Op::Jmp(105), Op::Jmp(269), Op::Jmp(326), Op::Jmp(516), Op::Acc(32), Op::Nop(147), Op::Jmp(-27), Op::Jmp(1), Op::Acc(-8), Op::Jmp(376), Op::Acc(-13), Op::Acc(0), Op::Acc(43), Op::Nop(380), Op::Jmp(230), Op::Acc(34), Op::Jmp(130), Op::Acc(18), Op::Acc(0), Op::Jmp(402), Op::Acc(31), Op::Acc(-1), Op::Acc(-5), Op::Jmp(134), Op::Jmp(334), Op::Acc(35), Op::Acc(0), Op::Acc(5), Op::Acc(-10), Op::Jmp(-85), Op::Acc(5), Op::Nop(444), Op::Acc(10), Op::Jmp(-9), Op::Acc(46), Op::Acc(-12), Op::Nop(98), Op::Acc(29), Op::Jmp(119), Op::Acc(8), Op::Acc(21), Op::Jmp(422), Op::Acc(19), Op::Jmp(78), Op::Acc(42), Op::Acc(18), Op::Nop(344), Op::Nop(353), Op::Jmp(26), Op::Acc(-16), Op::Acc(20), Op::Jmp(370), Op::Acc(-5), Op::Acc(29), Op::Jmp(465), Op::Nop(176), Op::Acc(-13), Op::Acc(-16), Op::Jmp(300), Op::Acc(12), Op::Acc(43), Op::Acc(-1), Op::Jmp(215), Op::Nop(214), Op::Acc(13), Op::Jmp(141), Op::Acc(-3), Op::Acc(42), Op::Acc(5), Op::Jmp(49), Op::Acc(7), Op::Acc(7), Op::Nop(2), Op::Jmp(5), Op::Nop(123), Op::Nop(112), Op::Jmp(45), Op::Jmp(276), Op::Acc(4), Op::Acc(5), Op::Acc(13), Op::Jmp(-97), Op::Jmp(311), Op::Nop(347), Op::Acc(6), Op::Jmp(1), Op::Jmp(162), Op::Acc(36), Op::Acc(-6), Op::Jmp(386), Op::Acc(-10), Op::Acc(-8), Op::Jmp(163), Op::Acc(32), Op::Acc(13), Op::Jmp(1), Op::Jmp(361), Op::Acc(43), Op::Acc(6), Op::Acc(31), Op::Jmp(52), Op::Acc(23), Op::Acc(34), Op::Nop(186), Op::Jmp(268), Op::Nop(-103), Op::Acc(-17), Op::Jmp(242), Op::Acc(30), Op::Acc(-4), Op::Jmp(-32), Op::Acc(27), Op::Acc(-17), Op::Jmp(-142), Op::Acc(30), Op::Acc(17), Op::Jmp(1), Op::Jmp(415), Op::Jmp(-132), Op::Acc(15), Op::Jmp(176), Op::Acc(15), Op::Acc(12), Op::Nop(382), Op::Jmp(237), Op::Jmp(32), Op::Acc(-8), Op::Acc(40), Op::Acc(28), Op::Jmp(1), Op::Jmp(-186), Op::Acc(9), Op::Acc(49), Op::Jmp(-55), Op::Acc(-16), Op::Acc(-7), Op::Nop(240), Op::Acc(29), Op::Jmp(255), Op::Jmp(182), Op::Acc(-16), Op::Acc(9), Op::Jmp(-31), Op::Acc(-13), Op::Acc(29), Op::Jmp(387), Op::Acc(-13), Op::Nop(-180), Op::Acc(-11), Op::Jmp(77), Op::Acc(16), Op::Jmp(368), Op::Jmp(224), Op::Acc(32), Op::Nop(-187), Op::Acc(48), Op::Jmp(307), Op::Acc(11), Op::Acc(38), Op::Nop(47), Op::Jmp(-94), Op::Jmp(1), Op::Nop(-170), Op::Acc(31), Op::Jmp(-180), Op::Acc(30), Op::Acc(1), Op::Jmp(1), Op::Nop(-63), Op::Jmp(-12), Op::Acc(-4), Op::Acc(-12), Op::Acc(15), Op::Nop(-68), Op::Jmp(13), Op::Acc(24), Op::Nop(-50), Op::Acc(31), Op::Acc(-2), Op::Jmp(333), Op::Acc(39), Op::Nop(-179), Op::Jmp(158), Op::Acc(24), Op::Jmp(169), Op::Acc(-3), Op::Jmp(-207), Op::Acc(-13), Op::Jmp(-54), Op::Acc(31), Op::Jmp(-93), Op::Acc(-4), Op::Acc(40), Op::Jmp(-96), Op::Acc(-15), Op::Acc(31), Op::Jmp(68), Op::Acc(38), Op::Acc(7), Op::Acc(12), Op::Jmp(-9), Op::Acc(49), Op::Acc(33), Op::Acc(27), Op::Acc(36), Op::Jmp(50), Op::Jmp(208), Op::Jmp(1), Op::Acc(42), Op::Acc(34), Op::Jmp(-151), Op::Acc(17), Op::Jmp(-195), Op::Acc(37), Op::Acc(34), Op::Jmp(62), Op::Jmp(1), Op::Acc(9), Op::Acc(3), Op::Acc(-2), Op::Jmp(266), Op::Nop(254), Op::Nop(-170), Op::Nop(-133), Op::Acc(40), Op::Jmp(225), Op::Acc(38), Op::Acc(33), Op::Acc(39), Op::Jmp(262), Op::Jmp(-278), Op::Acc(-17), Op::Acc(16), Op::Nop(128), Op::Jmp(-116), Op::Acc(13), Op::Acc(49), Op::Acc(36), Op::Acc(33), Op::Jmp(-215), Op::Nop(-301), Op::Jmp(-197), Op::Acc(50), Op::Jmp(-37), Op::Acc(42), Op::Nop(-253), Op::Jmp(159), Op::Jmp(-142), Op::Acc(14), Op::Jmp(-123), Op::Acc(-7), Op::Acc(-13), Op::Acc(33), Op::Acc(42), Op::Jmp(232), Op::Acc(2), Op::Acc(26), Op::Acc(3), Op::Jmp(-112), Op::Acc(29), Op::Acc(-12), Op::Nop(-263), Op::Nop(114), Op::Jmp(7), Op::Jmp(157), Op::Acc(-7), Op::Acc(11), Op::Nop(245), Op::Acc(-2), Op::Jmp(-225), Op::Nop(120), Op::Jmp(-114), Op::Acc(-5), Op::Acc(22), Op::Nop(-122), Op::Acc(-11), Op::Jmp(-70), Op::Acc(1), Op::Acc(24), Op::Acc(23), Op::Acc(37), Op::Jmp(188), Op::Acc(0), Op::Acc(-10), Op::Jmp(1), Op::Jmp(-283), Op::Jmp(-80), Op::Acc(4), Op::Jmp(-183), Op::Acc(-16), Op::Nop(-306), Op::Jmp(-213), Op::Acc(10), Op::Acc(-2), Op::Nop(-17), Op::Jmp(146), Op::Acc(-8), Op::Acc(5), Op::Acc(19), Op::Acc(37), Op::Jmp(-261), Op::Acc(28), Op::Acc(49), Op::Jmp(111), Op::Acc(37), Op::Acc(44), Op::Acc(20), Op::Jmp(-11), Op::Jmp(-53), Op::Acc(25), Op::Jmp(-343), Op::Acc(7), Op::Acc(46), Op::Jmp(-187), Op::Acc(20), Op::Acc(50), Op::Acc(-8), Op::Jmp(-365), Op::Nop(-9), Op::Acc(-18), Op::Jmp(-43), Op::Nop(165), Op::Nop(78), Op::Acc(33), Op::Acc(19), Op::Jmp(-321), Op::Acc(46), Op::Jmp(-275), Op::Nop(-88), Op::Acc(4), Op::Acc(33), Op::Acc(47), Op::Jmp(-18), Op::Jmp(166), Op::Jmp(1), Op::Acc(-4), Op::Acc(-9), Op::Acc(-2), Op::Jmp(-173), Op::Jmp(54), Op::Acc(-3), Op::Acc(2), Op::Nop(16), Op::Acc(-13), Op::Jmp(184), Op::Acc(26), Op::Nop(-322), Op::Acc(-12), Op::Jmp(-362), Op::Jmp(-118), Op::Acc(7), Op::Acc(33), Op::Jmp(153), Op::Jmp(-13), Op::Acc(19), Op::Jmp(1), Op::Acc(23), Op::Jmp(-373), Op::Acc(12), Op::Jmp(-184), Op::Jmp(-185), Op::Jmp(-57), Op::Acc(48), Op::Acc(8), Op::Nop(71), Op::Acc(26), Op::Jmp(-96), Op::Jmp(-227), Op::Acc(-10), Op::Jmp(-381), Op::Jmp(75), Op::Jmp(74), Op::Jmp(-320), Op::Acc(0), Op::Nop(101), Op::Jmp(-98), Op::Acc(33), Op::Acc(-4), Op::Jmp(1), Op::Acc(-9), Op::Jmp(-197), Op::Acc(36), Op::Acc(15), Op::Acc(24), Op::Jmp(-400), Op::Acc(18), Op::Jmp(-77), Op::Acc(25), Op::Acc(1), Op::Jmp(-112), Op::Nop(-150), Op::Jmp(-381), Op::Jmp(-152), Op::Acc(38), Op::Acc(50), Op::Acc(43), Op::Jmp(103), Op::Nop(-4), Op::Acc(-6), Op::Jmp(-309), Op::Acc(34), Op::Acc(2), Op::Acc(-15), Op::Jmp(-411), Op::Jmp(-70), Op::Acc(39), Op::Acc(-3), Op::Acc(6), Op::Acc(22), Op::Jmp(-123), Op::Jmp(-89), Op::Acc(11), Op::Jmp(70), Op::Jmp(-339), Op::Acc(-4), Op::Jmp(-325), Op::Acc(44), Op::Acc(8), Op::Acc(15), Op::Acc(29), Op::Jmp(87), Op::Jmp(-411), Op::Acc(30), Op::Jmp(12), Op::Acc(-14), Op::Jmp(-14), Op::Acc(-17), Op::Jmp(1), Op::Acc(-12), Op::Jmp(-441), Op::Jmp(1), Op::Acc(0), Op::Acc(-12), Op::Jmp(108), Op::Jmp(-277), Op::Jmp(103), Op::Acc(12), Op::Nop(-427), Op::Acc(10), Op::Acc(-16), Op::Jmp(-322), Op::Acc(1), Op::Jmp(-412), Op::Acc(37), Op::Jmp(-130), Op::Nop(-474), Op::Jmp(86), Op::Acc(5), Op::Acc(-12), Op::Jmp(-461), Op::Acc(-18), Op::Acc(-12), Op::Acc(30), Op::Nop(-356), Op::Jmp(-30), Op::Nop(-207), Op::Jmp(-128), Op::Nop(-168), Op::Acc(-4), Op::Jmp(-98), Op::Acc(32), Op::Nop(-264), Op::Jmp(-5), Op::Nop(-337), Op::Acc(-10), Op::Nop(-195), Op::Nop(62), Op::Jmp(-37), Op::Jmp(-489), Op::Jmp(-148), Op::Acc(50), Op::Acc(33), Op::Acc(8), Op::Acc(49), Op::Jmp(-353), Op::Acc(1), Op::Nop(-13), Op::Acc(27), Op::Jmp(-492), Op::Jmp(1), Op::Acc(43), Op::Jmp(-46), Op::Acc(-16), Op::Jmp(-149), Op::Acc(28), Op::Jmp(-525), Op::Acc(48), Op::Jmp(-30), Op::Acc(-5), Op::Acc(21), Op::Jmp(-15), Op::Jmp(1), Op::Acc(17), Op::Acc(42), Op::Acc(36), Op::Jmp(-343), Op::Acc(-7), Op::Acc(3), Op::Jmp(-346), Op::Acc(44), Op::Acc(18), Op::Acc(-10), Op::Nop(-262), Op::Jmp(-338), Op::Jmp(-111), Op::Jmp(-105), Op::Jmp(-319), Op::Acc(-11), Op::Jmp(-297), Op::Acc(1), Op::Acc(-3), Op::Jmp(-271), Op::Acc(15), Op::Acc(6), Op::Acc(24), Op::Jmp(-80), Op::Nop(-477), Op::Acc(39), Op::Jmp(-49), Op::Nop(-62), Op::Acc(23), Op::Acc(15), Op::Jmp(-47), Op::Acc(16), Op::Acc(5), Op::Acc(11), Op::Acc(42), Op::Jmp(-430), Op::Acc(14), Op::Acc(-16), Op::Jmp(-80), Op::Jmp(-571), Op::Acc(46), Op::Acc(31), Op::Jmp(1), Op::Acc(31), Op::Jmp(13), Op::Jmp(-5), Op::Jmp(-599), Op::Acc(41), Op::Jmp(-105), Op::Jmp(1), Op::Jmp(1), Op::Nop(-360), Op::Jmp(-542), Op::Acc(-5), Op::Acc(20), Op::Nop(-595), Op::Jmp(-124), Op::Acc(14), Op::Acc(40), Op::Acc(14), Op::Acc(34), Op::Jmp(1)];

fn run_inst_changed(changed_ip: i32) -> Option<i32> {
  let (mut acc, mut ip) = (0,0);
  let mut visited = [false; INPUT_LEN];

  while !visited[ip as usize] {
    visited[ip as usize] = true;
    match (&INPUT[ip as usize], ip == changed_ip) {
      (Op::Acc(n), _)     => acc += n,
      (Op::Jmp(n), false) => ip += n - 1,
      (Op::Nop(n), true)  => ip += n - 1,
      _ => {},
    }
    ip += 1;

    if ip == INPUT_LEN as i32 {
      return Some(acc);
    }
  }
  None
}

fn part_one() -> i32 {
  let (mut acc, mut ip) = (0,0);
  let mut visited = [false; INPUT_LEN];

  while !visited[ip as usize] {
    visited[ip as usize] = true;
    match INPUT[ip as usize] {
      Op::Acc(n) => acc += n,
      Op::Jmp(n) => ip += n - 1,
      Op::Nop(_) => {},
    }
    ip += 1;
  }
  acc
}

aoc2020::main! {
  let part_two = (0..INPUT_LEN)
    .filter(|&i| match INPUT[i] {
      Op::Acc(_) => false,
      Op::Jmp(_) => true,
      Op::Nop(_) => true,
    })
    .filter_map(|i| run_inst_changed(i as i32))
    .next()
    .unwrap();
  (part_one(), part_two)
}
