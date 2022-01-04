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
  actions = sorted(actions)

  slept = defaultdict[str, Counter[int]](Counter)
  asleep, guard = 0, ""
  for _, _, _, minute, action in actions:
    if action == 'up':
      slept[guard].update(range(asleep, minute))
    elif action == 'asleep':
      asleep = minute
    else:
      guard = action[1:]

  g1 = max(slept, key=lambda g: sum(slept[g].values()))
  g2 = max(slept, key=lambda g: slept[g].most_common(1)[0][1])
  return [int(g) * slept[g].most_common(1)[0][0] for g in [g1,g2]]

if __name__ == "__main__":
  main()
