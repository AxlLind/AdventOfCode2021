from aoc import main
from collections import defaultdict

INPUT = "Step J must be finished before step E can begin.\nStep X must be finished before step G can begin.\nStep D must be finished before step A can begin.\nStep K must be finished before step M can begin.\nStep P must be finished before step Z can begin.\nStep F must be finished before step O can begin.\nStep B must be finished before step I can begin.\nStep U must be finished before step W can begin.\nStep A must be finished before step R can begin.\nStep E must be finished before step R can begin.\nStep H must be finished before step C can begin.\nStep O must be finished before step S can begin.\nStep Q must be finished before step Y can begin.\nStep V must be finished before step W can begin.\nStep T must be finished before step N can begin.\nStep S must be finished before step I can begin.\nStep Y must be finished before step W can begin.\nStep Z must be finished before step C can begin.\nStep M must be finished before step L can begin.\nStep L must be finished before step W can begin.\nStep N must be finished before step I can begin.\nStep I must be finished before step G can begin.\nStep C must be finished before step G can begin.\nStep G must be finished before step R can begin.\nStep R must be finished before step W can begin.\nStep Z must be finished before step R can begin.\nStep Z must be finished before step N can begin.\nStep G must be finished before step W can begin.\nStep L must be finished before step G can begin.\nStep Y must be finished before step R can begin.\nStep P must be finished before step I can begin.\nStep C must be finished before step W can begin.\nStep T must be finished before step G can begin.\nStep T must be finished before step R can begin.\nStep V must be finished before step Z can begin.\nStep L must be finished before step C can begin.\nStep K must be finished before step I can begin.\nStep J must be finished before step I can begin.\nStep Q must be finished before step C can begin.\nStep F must be finished before step A can begin.\nStep H must be finished before step Y can begin.\nStep M must be finished before step N can begin.\nStep P must be finished before step H can begin.\nStep M must be finished before step C can begin.\nStep V must be finished before step Y can begin.\nStep O must be finished before step V can begin.\nStep O must be finished before step Q can begin.\nStep A must be finished before step G can begin.\nStep T must be finished before step Z can begin.\nStep K must be finished before step R can begin.\nStep H must be finished before step O can begin.\nStep O must be finished before step Y can begin.\nStep O must be finished before step C can begin.\nStep K must be finished before step P can begin.\nStep P must be finished before step F can begin.\nStep E must be finished before step M can begin.\nStep M must be finished before step I can begin.\nStep T must be finished before step W can begin.\nStep P must be finished before step L can begin.\nStep A must be finished before step O can begin.\nStep X must be finished before step V can begin.\nStep S must be finished before step G can begin.\nStep A must be finished before step Y can begin.\nStep J must be finished before step R can begin.\nStep K must be finished before step F can begin.\nStep J must be finished before step A can begin.\nStep P must be finished before step C can begin.\nStep E must be finished before step N can begin.\nStep F must be finished before step Y can begin.\nStep J must be finished before step D can begin.\nStep H must be finished before step Z can begin.\nStep U must be finished before step H can begin.\nStep J must be finished before step T can begin.\nStep V must be finished before step G can begin.\nStep Z must be finished before step I can begin.\nStep H must be finished before step W can begin.\nStep B must be finished before step R can begin.\nStep F must be finished before step B can begin.\nStep X must be finished before step C can begin.\nStep L must be finished before step R can begin.\nStep F must be finished before step U can begin.\nStep D must be finished before step N can begin.\nStep P must be finished before step O can begin.\nStep B must be finished before step O can begin.\nStep F must be finished before step C can begin.\nStep H must be finished before step L can begin.\nStep O must be finished before step N can begin.\nStep J must be finished before step Y can begin.\nStep H must be finished before step N can begin.\nStep O must be finished before step L can begin.\nStep I must be finished before step W can begin.\nStep J must be finished before step H can begin.\nStep D must be finished before step Z can begin.\nStep F must be finished before step W can begin.\nStep X must be finished before step W can begin.\nStep Y must be finished before step M can begin.\nStep T must be finished before step M can begin.\nStep U must be finished before step G can begin.\nStep L must be finished before step I can begin.\nStep N must be finished before step W can begin.\nStep E must be finished before step C can begin."

def path(reqs):
  left, ans = sorted(reqs.keys()), ""
  while left:
    ans += next(x for x in left if not any(r in left for r in reqs[x]))
    left.remove(ans[-1])
  return ans

def part2(reqs):
  left, working, t = sorted(reqs.keys()), {}, -1
  while left:
    workers = sorted(x for x in working.keys())
    for c in workers:
      working[c] -= 1
      if working[c] == 0:
        working.pop(c)
        left.remove(c)
    for x in left:
      if x in working or any(r in left for r in reqs[x]):
        continue
      if len(working) < 5:
        working[x] = 60 + ord(x) - ord('A') + 1
    t += 1
  return t

def solve():
  reqs = defaultdict(list)
  for l in INPUT.split('\n'):
    words = l.split(' ')
    reqs[words[7]].append(words[1])
    reqs[words[1]] # to force the key to be added
  return path(reqs), part2(reqs)

main(solve)
