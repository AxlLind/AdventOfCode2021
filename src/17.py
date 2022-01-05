import aoc
import re
import sys
from collections import defaultdict
sys.setrecursionlimit(10000)

def fill(m: dict[tuple[int,int],str], x: int, y: int, d: int) -> None:
  if m[x,y] == '|':
    fill(m,x+d,y,d)
    m[x,y] = '~'

def flow(m: dict[tuple[int,int],str], ymax: int, x: int, y: int, d: int) -> bool:
  if y > ymax:
    return True
  if m[x,y] != '.':
    return m[x,y] == '|'

  m[x,y] = '|'
  if flow(m,ymax,x,y+1,0):
    return True

  if (d != 1 and flow(m,ymax,x-1,y,-1)) | (d != -1 and flow(m,ymax,x+1,y,1)):
    return True

  if d == 0:
    fill(m,x,y,-1)
    fill(m,x+1,y,1)
  return False

@aoc.main('17')
def main(indata: str) -> tuple[int,int]:
  m = defaultdict(lambda: '.')
  for l in indata.split('\n'):
    s,a,b = [int(i) for i in re.findall("\d+",l)]
    hori = l[0] == 'x'
    for t in range(a,b+1):
      if hori:
        m[s,t] = '#'
      else:
        m[t,s] = '#'
  ymin,ymax = min(y for _,y in m), max(y for _,y in m)
  flow(m,ymax,500,0,0)
  water = sum(m[x,y] == '~' for x,y in m if y >= ymin)
  flows = sum(m[x,y] == '|' for x,y in m if y >= ymin)
  return water+flows, water

if __name__ == "__main__":
  main()
