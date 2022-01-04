import aoc
from itertools import cycle

@aoc.main('01')
def main(indata: str) -> tuple[int,int]:
  nums = [int(line) for line in indata.split('\n')]
  seen, s = set(), 0
  for x in cycle(nums):
    s += x
    if s in seen:
      break
    seen.add(s)
  return sum(nums), s

if __name__ == "__main__":
  main()
