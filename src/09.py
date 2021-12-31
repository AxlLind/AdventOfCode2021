from aoc import main
from collections import defaultdict, deque

INPUT = (455, 71223) # 455 players; last marble is worth 71223 points

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

def solve() -> tuple[int,int]:
  p1 = play(INPUT[0], INPUT[1])
  p2 = play(INPUT[0], INPUT[1]*100)
  return p1,p2

main(solve)
