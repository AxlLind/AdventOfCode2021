import aoc
from collections import defaultdict, deque

class RegexType:
  SIMPLE = 0
  OR = 1
  LIST = 2

Regex = tuple[RegexType, str | list['Regex']]
Graph = dict[tuple[int,int],set[tuple[int,int]]]

def parse_regexor(s: str, i: int) -> tuple[int,Regex]:
  regexes = []
  while i < len(s) and s[i] != ')':
    if s[i] == '|':
      i += 1
    i,r = parse_regex(s,i)
    regexes.append(r)
  return i+1, (RegexType.OR, regexes)

def parse_regex(s: str, i: int) -> tuple[int,Regex]:
  r = []
  while i < len(s) and s[i] not in "|)":
    if s[i] == '(':
      i,r1 = parse_regexor(s,i+1)
    else:
      iprev = i
      while i < len(s) and s[i] in "NSEW":
        i += 1
      r1 = (RegexType.SIMPLE, s[iprev:i])
    r.append(r1)
  return i, (RegexType.LIST, r)

def apply_regex(m: Graph, regex: Regex, coords: set[tuple[int,int]]) -> set[tuple[int,int]]:
  match regex[0]:
    case RegexType.SIMPLE:
      newcoords = set()
      for r,c in coords:
        for d in regex[1]:
          rr,cc = r,c
          match d:
            case 'N': r -= 1
            case 'S': r += 1
            case 'E': c += 1
            case 'W': c -= 1
          m[rr,cc].add((r,c))
          m[r,c].add((rr,cc))
        newcoords.add((r,c))
      return newcoords
    case RegexType.OR:
      return set.union(*(apply_regex(m,re,coords) for re in regex[1]))
    case RegexType.LIST:
      for re in regex[1]:
        coords = apply_regex(m,re,coords)
      return coords

def bfs_distances(m: Graph) -> list[int]:
  q, dist = deque([(0,0)]), {(0,0): 0}
  while q:
    r,c = q.popleft()
    for rr,cc in m[r,c]:
      if (rr,cc) in dist:
        continue
      dist[rr,cc] = dist[r,c] + 1
      q.append((rr,cc))
  return dist.values()

@aoc.main('20')
def main(indata: str) -> tuple[int,int]:
  _, regex = parse_regex(indata[1:-1], 0)
  m = defaultdict(set)
  apply_regex(m,regex,[(0,0)])
  distances = bfs_distances(m)
  return max(distances), sum(d >= 1000 for d in distances)

if __name__ == "__main__":
  main()
