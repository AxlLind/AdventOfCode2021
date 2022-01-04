import aoc
from collections import defaultdict, deque

Graph = dict[tuple[int,int],set[tuple[int,int]]]
Regex = str | tuple['Regex'] | list['Regex']

def parse_regexor(s: str, i: int) -> tuple[int,Regex]:
  regexor = []
  while i < len(s) and s[i] != ')':
    if s[i] == '|':
      i += 1
    i,r = parse_regex(s,i)
    regexor.append(r)
  return i+1, (regexor,)

def parse_regex(s: str, i: int) -> tuple[int,Regex]:
  r = []
  while i < len(s) and s[i] != '|' and s[i] != ')':
    if s[i] == '(':
      i,r1 = parse_regexor(s,i+1)
    else:
      iprev = i
      while i < len(s) and s[i] in "NSEW":
        i += 1
      r1 = s[iprev:i]
    r.append(r1)
  return i,r

def apply_regex(m: Graph, regex: Regex, coords: set[tuple[int,int]]) -> set[tuple[int,int]]:
  newcoords = set()

  if isinstance(regex,str):
    for r,c in coords:
      for d in regex:
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

  if isinstance(regex, tuple):
    for re in regex[0]:
      newcoords = newcoords.union(apply_regex(m,re,coords))
    return newcoords

  for re in regex:
    coords = apply_regex(m, re, coords)
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
