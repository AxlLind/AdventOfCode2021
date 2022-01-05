import aoc
from collections import Counter
from itertools import combinations

def part1(ids: list[str]) -> int:
  c = Counter()
  for l in ids:
    c.update(set(Counter(l).values()))
  return c[2]*c[3]

def part2(ids: list[str]) -> str:
  for a,b in combinations(ids,r=2):
    ans = [c1 for c1,c2 in zip(a,b) if c1 == c2]
    if len(ans)+1 == len(a):
      return ''.join(ans)
  assert False

@aoc.main('02')
def main(indata: str) -> tuple[int,str]:
  ids = indata.split('\n')
  return part1(ids), part2(ids)

if __name__ == "__main__":
  main()
