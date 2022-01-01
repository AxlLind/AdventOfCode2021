from aoc import main
from itertools import product

INPUT = 8979

def power(x: int, y: int) -> int:
  return (((x+10) * y + INPUT) * (x+10) // 100) % 10 - 5

def solve() -> tuple[str,str]:
  dp = [[[0]*300 for _ in range(300)] for _ in range(300)]
  for x,y in product(range(1,300), r=2):
    dp[x][y][1] = power(x,y)
  for size in range(2,300):
    for x,y in product(range(1,300-size), r=2):
      dp[x][y][size] = dp[x][y][1] \
                     + dp[x+1][y][size-1] \
                     + dp[x][y+1][size-1] \
                     - dp[x+1][y+1][size-2] \
                     + dp[x+size-1][y+size-1][1]

  x1,y1,n1,x2,y2,s2,n2 = 0,0,0,0,0,0,0
  for size in range(3,300):
    for x in range(1,300-size):
      for y in range(1,300-size):
        if size == 3:
          if dp[x][y][size] > n1:
            x1,y1,n1 = x,y,dp[x][y][size]
        else:
          if dp[x][y][size] > n2:
            x2,y2,s2,n2 = x,y,size,dp[x][y][size]
  return f"{x1},{y1}", f"{x2},{y2},{s2}"

main(solve)
