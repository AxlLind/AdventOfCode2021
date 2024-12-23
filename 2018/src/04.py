import aoc
from collections import defaultdict, Counter
import re

@aoc.main('04')
def main(indata: str) -> list[int]:
  actions = []
  for l in indata.split('\n'):
    xs = re.findall("\[1518-(\d+)-(\d+) (\d+):(\d+)\] (.+)", l)[0]
    month, day, hour, minute = [int(i) for i in xs[:4]]
    actions.append([month, day, hour, minute, xs[4].split(' ')[1]])
  asleep, guard, slept = 0, -1, defaultdict[int, Counter[int]](Counter)
  for _, _, _, minute, action in sorted(actions):
    match action:
      case 'up':
        slept[guard].update(range(asleep, minute))
      case 'asleep':
        asleep = minute
      case _:
        guard = int(action[1:])
  g1 = max(slept, key=lambda g: sum(slept[g].values()))
  g2 = max(slept, key=lambda g: slept[g].most_common(1)[0][1])
  return [g * slept[g].most_common(1)[0][0] for g in [g1,g2]]

if __name__ == "__main__":
  main()
