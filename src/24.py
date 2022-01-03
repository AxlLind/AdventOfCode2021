import aoc
import re
from dataclasses import dataclass
from copy import deepcopy

INPUT = "Immune System:\n123 units each with 8524 hit points with an attack that does 612 slashing damage at initiative 11\n148 units each with 4377 hit points (weak to slashing, bludgeoning) with an attack that does 263 cold damage at initiative 1\n6488 units each with 2522 hit points (weak to fire) with an attack that does 3 bludgeoning damage at initiative 19\n821 units each with 8034 hit points (immune to cold, bludgeoning) with an attack that does 92 cold damage at initiative 17\n1163 units each with 4739 hit points (weak to cold) with an attack that does 40 bludgeoning damage at initiative 14\n1141 units each with 4570 hit points (weak to fire, slashing) with an attack that does 32 radiation damage at initiative 18\n108 units each with 2954 hit points with an attack that does 262 radiation damage at initiative 8\n4752 units each with 6337 hit points (weak to bludgeoning, cold; immune to slashing) with an attack that does 13 cold damage at initiative 20\n4489 units each with 9894 hit points (weak to slashing) with an attack that does 20 slashing damage at initiative 12\n331 units each with 12535 hit points with an attack that does 300 slashing damage at initiative 15\n\nInfection:\n853 units each with 13840 hit points (weak to bludgeoning, cold) with an attack that does 26 fire damage at initiative 3\n450 units each with 62973 hit points (weak to slashing) with an attack that does 220 fire damage at initiative 13\n3777 units each with 35038 hit points (weak to cold) with an attack that does 18 radiation damage at initiative 7\n96 units each with 43975 hit points (immune to bludgeoning; weak to cold, slashing) with an attack that does 862 radiation damage at initiative 16\n1536 units each with 14280 hit points (weak to cold, fire; immune to bludgeoning) with an attack that does 18 slashing damage at initiative 2\n3696 units each with 36133 hit points (weak to radiation; immune to cold, fire) with an attack that does 18 bludgeoning damage at initiative 10\n3126 units each with 39578 hit points (weak to cold) with an attack that does 22 radiation damage at initiative 4\n1128 units each with 13298 hit points (weak to bludgeoning, slashing) with an attack that does 23 fire damage at initiative 6\n7539 units each with 6367 hit points (weak to fire; immune to radiation) with an attack that does 1 slashing damage at initiative 5\n1886 units each with 45342 hit points (weak to fire, cold) with an attack that does 45 cold damage at initiative 9"
PARSE_REGEX = "(\d+) units each with (\d+) hit points( \(.*\))? with an attack that does (\d+) ([a-z]+) damage at initiative (\d+)"

@dataclass
class Unit:
  units: int
  hp: int
  dmg: int
  dmgtype: str
  initiative: int
  weaknesses: set[str]
  immunities: set[str]

  @staticmethod
  def parse(s: str) -> 'Unit':
    units, hp, resistances, dmg, dmgtype, initiative = re.findall(PARSE_REGEX, s)[0]
    weaknesses, immunities = set(), set()
    if resistances:
      for s in resistances[2:-1].split(';'):
        t, _, *types = [w.rstrip(',') for w in s.strip().split(' ')]
        if t == "weak":
          weaknesses = set(types)
        if t == "immune":
          immunities = set(types)
    return Unit(int(units), int(hp), int(dmg), dmgtype, int(initiative), weaknesses, immunities)

  def effective_power(self) -> int:
    return self.units * self.dmg

  def dmg_against(self, u: 'Unit') -> int:
    if self.dmgtype in u.immunities:
      return 0
    if self.dmgtype in u.weaknesses:
      return self.dmg * 2 * self.units
    return self.dmg * self.units

def choose_targets(attackers: list[Unit], defenders: list[Unit]) -> dict[int,int]:
  targets = dict[int,int]()
  for i,u in enumerate(attackers):
    maxdmg,target = -1,-1
    for j,u2 in enumerate(defenders):
      if j in targets:
        continue
      dmg = u.dmg_against(u2)
      if maxdmg < dmg:
        maxdmg,target = dmg,j
    if maxdmg > 0:
      targets[target] = i
  return targets

def fight(teams: list[list[Unit]]) -> tuple[int,int]:
  teams, rounds = deepcopy(teams), 0
  while all(teams):
    if rounds > 10000:
      return -1,-1
    teams[0].sort(key=lambda u: (-u.effective_power(), -u.initiative))
    teams[1].sort(key=lambda u: (-u.effective_power(), -u.initiative))
    t0 = [(0,i,j) for j,i in choose_targets(teams[0],teams[1]).items()]
    t1 = [(1,i,j) for j,i in choose_targets(teams[1],teams[0]).items()]
    for t,i,j in sorted(t0+t1, key=lambda x: -teams[x[0]][x[1]].initiative):
      attacker, target = teams[t][i], teams[[1,0][t]][j]
      if attacker.units > 0:
        target.units -= attacker.dmg_against(target) // target.hp
    teams[0] = [u for u in teams[0] if u.units > 0]
    teams[1] = [u for u in teams[1] if u.units > 0]
    rounds += 1
  winner = 0 if teams[0] else 1
  return winner, sum(u.units for u in teams[winner])

@aoc.main
def main():
  teams = [[Unit.parse(l) for l in s.split('\n')[1:]] for s in INPUT.split("\n\n")]
  winner,p1 = fight(teams)
  while winner != 0:
    for u in teams[0]:
      u.dmg += 1
    winner,p2 = fight(teams)
  return p1,p2

if __name__ == "__main__":
  main()
