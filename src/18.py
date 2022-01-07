import aoc
from collections import Counter

def step(grid: list[str], times: int) -> list[str]:
  for _ in range(times):
    nextgrid = []
    for r in range(len(grid)):
      row = ""
      for c in range(len(grid[0])):
        ns = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]
        trees,yards = 0,0
        for r1,c1 in ns:
          if r1 < 0 or r1 >= len(grid) or c1 < 0 or c1 >= len(grid[0]):
            continue
          match grid[r1][c1]:
            case '|': trees += 1
            case '#': yards += 1
        match grid[r][c]:
          case '.': row += '|' if trees >= 3 else '.'
          case '|': row += '#' if yards >= 3 else '|'
          case '#': row += '#' if yards > 0 and trees > 0 else '.'
      nextgrid.append(row)
    grid = nextgrid
  return grid

def resource_value(grid: list[str]) -> int:
  c = Counter(tile for row in grid for tile in row)
  return c['#'] * c['|']

@aoc.main('18')
def main(indata: str) -> tuple[int,int]:
  grid = indata.split('\n')
  p1 = resource_value(step(grid,10))
  seen = {}
  for i in range(1000000000):
    grid = step(grid,1)
    s = ''.join(''.join(row) for row in grid)
    if s in seen:
      left = (1000000000 - seen[s] - 1) % (i-seen[s])
      grid = step(grid,left)
      break
    seen[s] = i
  return p1, resource_value(grid)

if __name__ == "__main__":
  main()
