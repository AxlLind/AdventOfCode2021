from aoc import main
from itertools import product
from collections import Counter
from typing import Iterable

INPUT = "260, 78\n42, 40\n87, 276\n219, 124\n166, 137\n341, 138\n82, 121\n114, 174\n218, 289\n61, 358\n328, 164\n279, 50\n218, 107\n273, 320\n192, 349\n354, 103\n214, 175\n128, 196\n237, 67\n333, 150\n98, 260\n166, 217\n92, 212\n55, 165\n205, 138\n321, 199\n285, 148\n217, 130\n357, 319\n160, 67\n63, 75\n345, 123\n316, 220\n41, 253\n240, 245\n201, 124\n336, 166\n95, 301\n55, 181\n219, 315\n209, 237\n317, 254\n314, 300\n242, 295\n295, 293\n285, 263\n330, 204\n112, 106\n348, 49\n81, 185"

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

def solve():
  points = [[int(i) for i in l.split(', ')] for l in INPUT.split('\n')]
  return part1(points), part2(points)

main(solve)
