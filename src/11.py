import aoc
from itertools import product

INPUT = 8979

def part1(dp: list[list[list[int]]]) -> str:
  xmax,ymax,v = 0,0,0
  for x,y in product(range(1,297), repeat=2):
    if dp[x][y][3] > v:
      xmax,ymax,v = x,y,dp[x][y][3]
  return f"{xmax},{ymax}"

def part2(dp: list[list[list[int]]]) -> str:
  xmax,ymax,sizemax,v = 0,0,0,0
  for size in range(2,300):
    for x,y in product(range(1,300-size), repeat=2):
      if dp[x][y][size] > v:
        xmax,ymax,sizemax,v = x,y,size,dp[x][y][size]
  return f"{xmax},{ymax},{sizemax}"

@aoc.main
def main() -> tuple[str,str]:
  dp = [[[0]*300 for _ in range(300)] for size in range(300)]
  for x,y in product(range(1,300), repeat=2):
    dp[x][y][1] = (((x+10) * y + INPUT) * (x+10) // 100) % 10 - 5
  for size in range(2,300):
    for x,y in product(range(1,300-size), repeat=2):
      dp[x][y][size] = dp[x][y][1] \
                     + dp[x+1][y][size-1] \
                     + dp[x][y+1][size-1] \
                     - dp[x+1][y+1][size-2] \
                     + dp[x+size-1][y+size-1][1]
  return part1(dp), part2(dp)

if __name__ == "__main__":
  main()
