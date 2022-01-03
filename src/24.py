import aoc
import re
from dataclasses import dataclass
from copy import deepcopy

INPUT = """Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"""
INPUT = "Immune System:\n123 units each with 8524 hit points with an attack that does 612 slashing damage at initiative 11\n148 units each with 4377 hit points (weak to slashing, bludgeoning) with an attack that does 263 cold damage at initiative 1\n6488 units each with 2522 hit points (weak to fire) with an attack that does 3 bludgeoning damage at initiative 19\n821 units each with 8034 hit points (immune to cold, bludgeoning) with an attack that does 92 cold damage at initiative 17\n1163 units each with 4739 hit points (weak to cold) with an attack that does 40 bludgeoning damage at initiative 14\n1141 units each with 4570 hit points (weak to fire, slashing) with an attack that does 32 radiation damage at initiative 18\n108 units each with 2954 hit points with an attack that does 262 radiation damage at initiative 8\n4752 units each with 6337 hit points (weak to bludgeoning, cold; immune to slashing) with an attack that does 13 cold damage at initiative 20\n4489 units each with 9894 hit points (weak to slashing) with an attack that does 20 slashing damage at initiative 12\n331 units each with 12535 hit points with an attack that does 300 slashing damage at initiative 15\n\nInfection:\n853 units each with 13840 hit points (weak to bludgeoning, cold) with an attack that does 26 fire damage at initiative 3\n450 units each with 62973 hit points (weak to slashing) with an attack that does 220 fire damage at initiative 13\n3777 units each with 35038 hit points (weak to cold) with an attack that does 18 radiation damage at initiative 7\n96 units each with 43975 hit points (immune to bludgeoning; weak to cold, slashing) with an attack that does 862 radiation damage at initiative 16\n1536 units each with 14280 hit points (weak to cold, fire; immune to bludgeoning) with an attack that does 18 slashing damage at initiative 2\n3696 units each with 36133 hit points (weak to radiation; immune to cold, fire) with an attack that does 18 bludgeoning damage at initiative 10\n3126 units each with 39578 hit points (weak to cold) with an attack that does 22 radiation damage at initiative 4\n1128 units each with 13298 hit points (weak to bludgeoning, slashing) with an attack that does 23 fire damage at initiative 6\n7539 units each with 6367 hit points (weak to fire; immune to radiation) with an attack that does 1 slashing damage at initiative 5\n1886 units each with 45342 hit points (weak to fire, cold) with an attack that does 45 cold damage at initiative 9"

PARSE_REGEX = "(\d+) units each with (\d+) hit points( \(.*\))? with an attack that does (\d+) ([a-z]+) damage at initiative (\d+)"

@dataclass
class Unit:
  units: int
  hp: int
  dmg: int
  dmgtype: str
  initiative: int
  weaknesses: list[str]
  immunities: list[str]
  boost: int = 0

  def parse(s: str) -> 'Unit':
    units, hp, resistances, dmg, dmgtype, initiative = re.findall(PARSE_REGEX, s)[0]
    weaknesses, immunities = [], []
    if resistances:
      for s in resistances[2:-1].split(';'):
        t, _, *types = [w.rstrip(',') for w in s.strip().split(' ')]
        if t == "weak":
          weaknesses = types
        if t == "immune":
          immunities = types
    return Unit(int(units), int(hp), int(dmg), dmgtype, int(initiative), weaknesses, immunities)

  def effective_power(self) -> int:
    return self.units * (self.dmg + self.boost)

  def dmg_against(self, u: 'Unit') -> int:
    if self.dmgtype in u.immunities:
      return 0
    if self.dmgtype in u.weaknesses:
      return (self.dmg + self.boost) * 2
    return (self.dmg + self.boost)

def choose_targets(attackers: list[Unit], defenders: list[Unit]) -> dict[int,int]:
  targets = {}
  for i,u in enumerate(attackers):
    maxdmg,target = -1,-1
    for j,u2 in enumerate(defenders):
      if j in targets.values():
        continue
      dmg = u.dmg_against(u2)
      if maxdmg < dmg:
        maxdmg,target = dmg,j
    if maxdmg > 0:
      targets[i] = target
  return targets

def fight(teams: list[list[Unit]]) -> int:
  rounds = 0
  while teams[0] and teams[1] and rounds < 100000:
    teams[0].sort(key=lambda u: (-u.effective_power(), -u.initiative))
    teams[1].sort(key=lambda u: (-u.effective_power(), -u.initiative))
    t0 = choose_targets(teams[0],teams[1])
    t1 = choose_targets(teams[1],teams[0])
    attack_order = sorted([(0,i,j) for i,j in t0.items()] + [(1,i,j) for i,j in t1.items()], key=lambda x: -teams[x[0]][x[1]].initiative)
    for t,i,j in attack_order:
      attacker = teams[t][i]
      target = teams[0 if t == 1 else 1][j]
      if attacker.units <= 0:
        continue
      target.units -= attacker.dmg_against(target) * attacker.units // target.hp
    teams[0] = [u for u in teams[0] if u.units > 0]
    teams[1] = [u for u in teams[1] if u.units > 0]
    rounds += 1
  if rounds == 100000:
    return -1,-1
  winner = 0 if teams[0] else 1
  return winner, sum(u.units for u in teams[winner])

@aoc.main
def main():
  teams = [[Unit.parse(l) for l in s.split('\n')[1:]] for s in INPUT.split("\n\n")]
  _, p1 = fight(deepcopy(teams))
  boost = 1
  while True:
    for u in teams[0]:
      u.boost = boost
    winner,p2 = fight(deepcopy(teams))
    if winner == 0:
      break
    boost += 1
  return p1,p2

if __name__ == "__main__":
  main()
