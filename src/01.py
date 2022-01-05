import aoc
from itertools import cycle

@aoc.main('01')
def main(indata: str) -> tuple[int,int]:
  nums, seen, s = [int(l) for l in indata.split('\n')], set(), 0
  for x in cycle(nums):
    s += x
    if s in seen:
      break
    seen.add(s)
  return sum(nums), s

if __name__ == "__main__":
  main()
