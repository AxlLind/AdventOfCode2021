import aoc
from itertools import product
from collections import defaultdict
import re

@aoc.main('03')
def main(indata: str) -> tuple[int,int]:
  squares = [[int(i) for i in re.findall("\d+",l)] for l in indata.split('\n')]
  points = defaultdict[tuple[int,int],int](int)
  for _,x,y,h,w in squares:
    for p in product(range(x,x+h), range(y,y+w)):
      points[p] += 1
  for p2,x,y,h,w in squares:
    if all(points[p] == 1 for p in product(range(x,x+h), range(y,y+w))):
      return sum(c > 1 for c in points.values()), p2

if __name__ == "__main__":
  main()
