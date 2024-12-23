import aoc
import re
from dataclasses import dataclass
from copy import deepcopy

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

@aoc.main('24')
def main(indata: str) -> tuple[int,int]:
  teams = [[Unit.parse(l) for l in s.split('\n')[1:]] for s in indata.split("\n\n")]
  winner,p1 = fight(teams)
  while winner != 0:
    for u in teams[0]:
      u.dmg += 1
    winner,p2 = fight(teams)
  return p1,p2

if __name__ == "__main__":
  main()
