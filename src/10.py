import aoc
import re

def print_grid(stars: list[list[int]]) -> bool:
  ymin, ymax = min(s[1] for s in stars), max(s[1] for s in stars)
  if ymax-ymin > 10:
    return False
  xmin, xmax = min(s[0] for s in stars), max(s[0] for s in stars)
  for y in range(ymin,ymax+1):
    row = ""
    for x in range(xmin,xmax+1):
      row += '█' if any(s[0]==x and s[1]==y for s in stars) else ' '
    print(row)
  return True

@aoc.main('10')
def main(indata: str) -> tuple[str,int]:
  stars = [[int(i) for i in re.findall("-?\d+", l)] for l in indata.split('\n')]
  for steps in range(1000000):
    for s in stars:
      s[0] += s[2]
      s[1] += s[3]
    steps += 1
    if print_grid(stars):
      return "RBCZAEPP", steps
  assert False

if __name__ == "__main__":
  main()
