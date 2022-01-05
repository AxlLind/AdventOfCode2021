import aoc
import re

def dist(s1: list[int], s2: list[int]) -> int:
  return sum(abs(a-b) for a,b in zip(s1,s2))

def remove_component(stars: list[list[int]], s: list[int]):
  if s not in stars:
    return
  stars.remove(s)
  for s2 in [s2 for s2 in stars if dist(s,s2) < 4]:
    remove_component(stars, s2)

@aoc.main('25')
def main(indata: str) -> tuple[int,str]:
  stars = [[int(i) for i in re.findall("-?\d+", l)] for l in indata.split('\n')]
  components = 0
  while stars:
    remove_component(stars, stars[0])
    components += 1
  return components, '🎄'

if __name__ == '__main__':
  main()
