import aoc

INPUT = "initial state: #...#...##..####..##.####.#...#...#.#.#.#......##....#....######.####.##..#..#..##.##..##....#######\n\n.#### => .\n...#. => .\n.##.. => #\n#.##. => .\n#..## => .\n##### => #\n####. => #\n.##.# => #\n#.### => .\n...## => #\n.#.## => #\n#..#. => #\n#.#.. => #\n.###. => #\n##.## => #\n##..# => .\n.#... => #\n###.# => .\n..##. => .\n..... => .\n###.. => #\n..#.# => .\n.#..# => #\n##... => #\n#.... => .\n##.#. => .\n..#.. => #\n....# => .\n#...# => .\n#.#.# => #\n..### => .\n.#.#. => #"

def step(transitions: set[str], pots: set[int]) -> set[int]:
  new_pots = set()
  for p in range(min(pots)-2,max(pots)+3):
    s = ''.join('#' if p+j in pots else '.' for j in range(-2,3))
    if s in transitions:
      new_pots.add(p)
  return new_pots

def part1(transitions: set[str], pots: set[int]) -> int:
  for i in range(20):
    pots = step(transitions, pots)
  return sum(pots)

def part2(transitions: set[str], pots: set[int]) -> int:
  seen = set()
  for n in range(1,1000):
    pots = step(transitions, pots)
    minpot = min(pots)
    pattern = tuple(sorted(p - minpot for p in pots))
    if pattern in seen:
      return (50000000000 - n) * len(pots) + sum(pots)
    seen.add(pattern)
  assert False

@aoc.main
def main() -> tuple[int,int]:
  state, rest = INPUT.split('\n\n')
  pots = {i for i,c in enumerate(state.split(": ").pop()) if c == '#'}
  transitions = {s[:5] for s in rest.split('\n') if s[-1] == '#'}
  return part1(transitions, pots), part2(transitions, pots)

if __name__ == "__main__":
  main()
