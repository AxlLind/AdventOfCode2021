import aoc
from collections import defaultdict

def path(reqs: dict[str,list[str]]) -> str:
  left, ans = sorted(reqs.keys()), ""
  while left:
    ans += next(x for x in left if not any(r in left for r in reqs[x]))
    left.remove(ans[-1])
  return ans

def part2(reqs: dict[str,list[str]]) -> int:
  left, working, t = sorted(reqs.keys()), dict[str,int](), 0
  while left:
    for c in list(working.keys()):
      working[c] -= 1
      if working[c] == 0:
        working.pop(c)
        left.remove(c)
    for x in left:
      if x in working or any(r in left for r in reqs[x]):
        continue
      if len(working) < 5:
        working[x] = ord(x) - 4
    t += 1
  return t-1

@aoc.main('07')
def main(indata: str):
  reqs = defaultdict(list)
  for l in indata.split('\n'):
    reqs[l[36]].append(l[5])
    reqs[l[5]] # to force the key to be added
  return path(reqs), part2(reqs)

if __name__ == "__main__":
  main()
