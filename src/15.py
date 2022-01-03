import aoc
from copy import deepcopy
from collections import deque, defaultdict
from itertools import product

INPUT = "################################\n#########....#..#####.......####\n###########G......###..##..#####\n###########.....#.###......#####\n###############.#...#.......####\n###############..#....E......###\n############.##...#...G....#####\n############.##.....G..E...#####\n###########G.##...GG......######\n#..####G##..G##..G.#......######\n#..........#............#.######\n#.......#....G.......G.##..#...#\n#.....G.......#####...####...#.#\n#.....G..#...#######..#####...E#\n#.##.....G..#########.#######..#\n#........G..#########.#######E##\n####........#########.##########\n##.#........#########.##########\n##.G....G...#########.##########\n##...........#######..##########\n#.G..#........#####...##########\n#......#.G.G..........##########\n###.#................###########\n###..................###.#######\n####............E.....#....#####\n####.####.......####....E.######\n####..#####.....####......######\n#############..#####......######\n#####################EE..E######\n#####################..#.E######\n#####################.##########\n################################"

def bfs(m: list[list[tuple[str,int]]], rs: int, cs: int, enemy: str) -> tuple[int,int] | None:
  q, parentmap = deque([(rs,cs)]), {(rs,cs): None}
  while q:
    r,c = q.popleft()
    if m[r][c][0] == enemy:
      while parentmap[parentmap[r,c]]:
        r,c = parentmap[r,c]
      return r,c
    for rr,cc in [(r-1,c),(r,c-1),(r,c+1),(r+1,c)]:
      if (rr,cc) in parentmap or m[rr][cc][0] not in ['.',enemy]:
        continue
      q.append((rr,cc))
      parentmap[rr,cc] = (r,c)

def get_target(m: list[list[tuple[str,int]]], r: int, c: int, enemy: str) -> tuple[int,int] | None:
  minhp, rt, ct = 1000, None, None
  for rr,cc in [(r-1,c),(r,c-1),(r,c+1),(r+1,c)]:
    if m[rr][cc][0] == enemy and m[rr][cc][1] < minhp:
        minhp,rt,ct = m[rr][cc][1],rr,cc
  return (rt,ct) if rt else None

def step(m: list[list[tuple[str,int]]], dmg: dict[str,int]):
  moved = set()
  for r,c in product(range(len(m)),range(len(m[0]))):
    if m[r][c][0] not in "GE" or (r,c) in moved:
      continue
    enemy = 'E' if m[r][c][0] == 'G' else 'G'
    target = get_target(m,r,c,enemy)
    if not target:
      s = bfs(m,r,c,enemy)
      if s:
        rt,ct = s
        m[rt][ct], m[r][c] = m[r][c], ('.',0)
        moved.add((rt,ct))
        target = get_target(m,rt,ct,enemy)
    if target:
      rt,ct = target
      m[rt][ct] = (enemy, m[rt][ct][1] - dmg[enemy])
      if m[rt][ct][1] <= 0:
        m[rt][ct] = ('.',0)

def count_left(m: list[list[tuple[str,int]]]) -> bool:
  counts = defaultdict(int)
  for r,c in product(range(len(m)),range(len(m[0]))):
    counts[m[r][c][0]] += 1
  return counts['E'],counts['G']

def simulate(dmg: dict[str,int], m: list[list[tuple[str,int]]], noelfdeath: bool = False) -> tuple[str,int]:
  m, rounds, e, g = deepcopy(m), 0, *count_left(m)
  while True:
    step(m,dmg)
    e,g,e2 = *count_left(m), e
    if e == 0 or g == 0:
      break
    if noelfdeath and e < e2:
      return 'G', -1
    rounds += 1

  winner, score = "", 0
  for r,c in product(range(len(m)),range(len(m[0]))):
    if m[r][c][0] in "GE":
      winner = m[r][c][0]
      score += m[r][c][1]
  return winner, score*rounds

@aoc.main
def main() -> tuple[int,int]:
  m = []
  for s in INPUT.split('\n'):
    row = []
    for c in s:
      hp = 200 if c in "GE" else 0
      row.append((c,hp))
    m.append(row)
  dmg = {'E': 3, 'G': 3}
  winner, p1 = simulate(dmg, m)
  while winner != 'E':
    dmg['G'] += 1
    winner, p2 = simulate(dmg,m,True)
  return p1,p2

if __name__ == "__main__":
  main()
