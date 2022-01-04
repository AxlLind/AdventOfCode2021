import aoc
from itertools import product
from collections import Counter
from typing import Iterable

def bounding_box(points: list[list[int]]) -> Iterable[tuple[int,int]]:
  xmin, xmax = min(x for x,_ in points), max(x for x,_ in points)
  ymin, ymax = min(y for _,y in points), max(y for _,y in points)
  yield from product(range(xmin,xmax+1), range(ymin,ymax+1))

def part1(points: list[list[int]]) -> int:
  closest = {}
  for x,y in bounding_box(points):
    distances = [abs(a-x) + abs(b-y) for a,b in points]
    d = min(distances)
    if distances.count(d) == 1:
      closest[(x,y)] = distances.index(d)
  return Counter(closest.values()).most_common(1)[0][1]

def part2(points: list[list[int]]) -> int:
  ans = 0
  for x,y in bounding_box(points):
    total_dist = sum(abs(a-x) + abs(b-y) for a,b in points)
    ans += total_dist < 10000
  return ans

@aoc.main('06')
def main(indata: str):
  points = [[int(i) for i in l.split(', ')] for l in indata.split('\n')]
  return part1(points), part2(points)

if __name__ == "__main__":
  main()
