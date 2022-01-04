import aoc
import re
from collections import defaultdict
from heapq import heappush, heappop
from itertools import product

Node = tuple[int,int,bool,bool]

def neighbors(cave: list[list[int]], node: Node) -> list[tuple[int,Node]]:
  r,c,torch,gear = node
  match cave[r][c]:
    case 0: ns = [(7,(r,c,not torch,not gear))]
    case 1: ns = [(7,(r,c,torch,not gear))]
    case 2: ns = [(7,(r,c,not torch,gear))]
  for r1,c1 in [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]:
    if r1 < 0 or r1 >= len(cave) or c1 < 0 or c1 >= len(cave[0]):
      continue
    match cave[r1][c1]:
      case 0:
        if torch or gear:
          ns.append((1,(r1,c1,torch,gear)))
      case 1:
        if not torch:
          ns.append((1,(r1,c1,torch,gear)))
      case 2:
        if not gear:
          ns.append((1,(r1,c1,torch,gear)))
  return ns

def dijkstra(cave: list[list[int]], start: Node, target: Node) -> int:
  q, costs = [(0,start)], defaultdict[Node,int](lambda: 100000)
  costs[start] = 0
  while q:
    cost,here = heappop(q)
    if here == target:
      return cost
    for c,node in neighbors(cave,here):
      c += cost
      if c < costs[node]:
        costs[node] = c
        heappush(q, (c,node))
  assert False

def build_cave(depth: int, rows: int, cols: int) -> tuple[int, list[list[int]]]:
  cave = [[0 for _ in range(2*cols)] for _ in range(2*rows)]
  for r in range(len(cave)):
    cave[r][0] = (r * 16807 + depth) % 20183
  for c in range(len(cave[0])):
    cave[0][c] = (c * 48271 + depth) % 20183
  for r,c in product(range(1,len(cave)), range(1,len(cave[0]))):
    cave[r][c] = (cave[r-1][c] * cave[r][c-1] + depth) % 20183
  cave[rows][cols] = depth % 20183
  risk = 0
  for r,c in product(range(len(cave)), range(len(cave[0]))):
    cave[r][c] %= 3
    if r <= rows and c <= cols:
      risk += cave[r][c]
  return risk,cave

@aoc.main('22')
def main(indata: str) -> tuple[int,int]:
  depth,rows,cols = [int(i) for i in re.findall("\d+", indata)]
  p1, cave = build_cave(depth,rows,cols)
  return p1, dijkstra(cave, (0,0,True,False), (rows,cols,True,False))

if __name__ == "__main__":
  main()
