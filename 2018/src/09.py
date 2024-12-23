import aoc
import re
from collections import defaultdict, deque

def play(players: int, rounds: int) -> int:
  marbles, scores = deque([0]), defaultdict[int,int](int)
  for i in range(1, rounds+1):
    if i % 23 == 0:
      marbles.rotate(7)
      scores[i % players] += i + marbles.pop()
      marbles.rotate(-1)
    else:
      marbles.rotate(-1)
      marbles.append(i)
  return max(scores.values())

@aoc.main('09')
def main(indata: str) -> tuple[int,int]:
  players, rounds = [int(i) for i in re.findall('\d+', indata)]
  p1 = play(players, rounds)
  p2 = play(players, rounds*100)
  return p1,p2

if __name__ == "__main__":
  main()
