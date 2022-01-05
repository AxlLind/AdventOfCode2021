import aoc
from typing import Any

Tree = tuple[list[Any],list[int]] # mypy cannot handle recursive types, use 'Any' for now

def parse_tree(xs: list[int], i: int) -> tuple[int,Tree]:
  children, ndata = [], xs[i+1]
  i += 2
  for _ in range(xs[i-2]):
    i, child = parse_tree(xs,i)
    children.append(child)
  return i+ndata, (children, xs[i:i+ndata])

def metadata_sum(e: Tree) -> int:
  return sum(e[1]) + sum(metadata_sum(c) for c in e[0])

def node_value(e: Tree) -> int:
  cs, data = e
  if len(cs) == 0:
    return sum(data)
  return sum(node_value(cs[c-1]) for c in data if 0 < c <= len(cs))

@aoc.main('08')
def main(indata: str) -> tuple[int,int]:
  _,e = parse_tree([int(i) for i in indata.split(' ')],0)
  return metadata_sum(e), node_value(e)

if __name__ == "__main__":
  main()
