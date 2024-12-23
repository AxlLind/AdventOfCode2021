import aoc
import re
from z3 import If, Int, Optimize

def part1(bots: list[list[int]]) -> int:
  x,y,z,r = max(bots, key=lambda b: b[3])
  return sum(abs(x1-x) + abs(y1-y) + abs(z1-z) <= r for x1,y1,z1,_ in bots)

def part2(bots: list[list[int]]) -> int:
  def abs(x):
    return If(x >= 0,x,-x)
  x,y,z,opt = Int("x"), Int("y"), Int("z"), Optimize()
  opt.maximize(sum(If(abs(x-x1) + abs(y-y1) + abs(z-z1) <= r, 1, 0) for x1,y1,z1,r in bots))
  opt.minimize(abs(x) + abs(y) + abs(z))
  opt.check()
  return opt.model().eval(abs(x) + abs(y) + abs(z))

@aoc.main('23')
def main(indata: str) -> tuple[int,int]:
  bots = [[int(i) for i in re.findall("-?\d+", l)] for l in indata.split('\n')]
  return part1(bots), part2(bots)

if __name__ == "__main__":
  main()
